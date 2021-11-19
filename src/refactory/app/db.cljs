(ns refactory.app.db
  (:require [datascript.core :as ds]
            [goog.functions :refer [debounce]]
            [malli.core :as m]
            [malli.error :as me]
            [posh.reagent]
            [re-frame.core :as rf]
            [re-frame.db]
            [re-posh.core :as rp]
            [re-posh.db]))


;;
;; DataScript
;;
;; DataScript holds all persistent application data. When we save our state,
;; this is what we're saving.
;;

;; Our full DataScript schema is here. This may include some of our own keys:
;;
;;   :app/default: Usable by queries to fill in consistent default values for
;;                 missing attributes.
;;   :app/malli: An optional malli schema to validate attribute values. These
;;               are only used to log validation warnings during development.
;;
(def ds-schema
  {;; Each page gets a record to store any page-global state.
   :page/id {:db/unique :db.unique/identity
             :app/malli :keyword}

   ;; For example, [:page/id :factories] will hold the currently selected
   ;; factory.
   :factories/selected {:db/valueType :db.type/ref
                        :app/malli :int}

   ;; Factories and their components (refactory.app.pages.factories).
   :factory/title {:db/index true
                   :app/malli [:string {:min 1, :max 30}]}
   :factory/mode {:app/default :continuous
                  :app/malli [:and :keyword [:enum :continuous :fixed]]}
   :factory/jobs {:db/valueType :db.type/ref
                  :db/cardinality :db.cardinality/many
                  :db/isComponent true}

   :job/recipe-id {:app/malli :string}
   :job/disabled? {:app/malli :boolean}
   ;; Number of iterations (for :fixed mode)
   :job/count {:app/default 1
               :app/malli 'nat-int?}
   ;; Distinct builders (for :continuous mode)
   :job/instances {:db/valueType :db.type/ref
                   :db/cardinality :db.cardinality/many
                   :db/isComponent true}

   :instance/overdrive {:app/default 100
                        :app/malli [:int {:min 0, :max 250}]}})


(defn ds
  "Convenience wrapper to the DataScript db, mainly for the REPL."
  []
  @@re-posh.db/store)


(defn attr->default
  [attr]
  (get-in ds-schema [attr :app/default]))


(defn attr->malli-schema
  [attr]
  (get-in ds-schema [attr :app/malli]))


(defn- validate-tx-report
  [{:keys [tx-data]}]
  (if (empty? tx-data)
    (js/console.warn "Transaction affected no datoms.")
    (doseq [[eid attr value _timestamp added?] tx-data]
      (when added?
        (when-some [schema (attr->malli-schema attr)]
          (when-not (m/validate schema value)
            (doseq [msg (me/humanize (m/explain schema value))]
              (js/console.warn (pr-str [eid attr value]) "- value" msg))))))))


(defn- tap-datoms
  [{:keys [tx-data]}]
  (doseq [datom tx-data]
    (tap> (vec datom))))


;;
;; Persistence
;;
;; On save, we use DataScript's serialization to store the entire database,
;; including the schema and indexes. On load, we deserialize the database and
;; then extract the datoms and combine them with the latest schema
;;

(def STORAGE-KEY "autosave")


(defn- save-to-storage!
  []
  (->> (ds)
       (ds/serializable)
       (js/JSON.stringify)
       (. js/window.localStorage setItem STORAGE-KEY)))


(defn- create-conn-from-storage
  []
  (when-some [data (. js/window.localStorage getItem STORAGE-KEY)]
    (try
      (-> data
          (js/JSON.parse)
          (ds/from-serializable)
          (ds/datoms :eavt)
          (ds/conn-from-datoms ds-schema))
      (catch js/Error e
        (js/console.warn "Failed to load saved database." e)))))


(def save-soon (debounce #(rf/dispatch [::save]) 2500))


(rf/reg-event-fx
  ::mark-dirty
  (fn [{:keys [db]} _]
    {:db (assoc db ::dirty? true)
     :fx [[::save-soon]]}))


(rf/reg-fx
  ::save-soon
  (fn [_]
    (save-soon)))


(rf/reg-event-fx
  ::save
  (fn [{:keys [db]} _]
    {:db (dissoc db ::dirty?)
     :fx [[::save]]}))


(rf/reg-fx
  ::save
  (fn [_]
    (save-to-storage!)))


(defn- mark-dirty
  [{:keys [tx-data]}]
  (when (seq tx-data)
    (rf/dispatch [::mark-dirty])))


(rf/reg-sub
  ::dirty?
  (fn [db _]
    (::dirty? db)))


;;
;; Transactions
;;

(defn tap-ds
  []
  (tap> (mapv (comp ds/touch (partial ds/entity (ds)))
              (ds/q '[:find [?e ...] :where [?e]] (ds)))))


(defn tap-app-db
  []
  (tap> @re-frame.db/app-db))


(comment
  (tap-ds)
  (tap-app-db))


(defn transact!
  "Convenience wrapper."
  ([datoms]
   (transact! datoms nil))
  ([datoms tx-meta]
   (posh.reagent/transact! @re-posh.db/store datoms tx-meta)))


(rf/reg-fx
  :transact+
  (fn [{:keys [datoms tx-meta on-success on-fail]}]
    (let [result (try (transact! datoms tx-meta)
                      (catch js/Error e e))]
      (if (instance? js/Error result)
        (if (vector? on-fail)
          (rf/dispatch (conj on-fail result))
          (throw result))
        (when (vector? on-success)
          (rf/dispatch (conj on-success result)))))))


;;
;; app-db
;;
;; Although DataScript holds all persistent data, we still use the app-db for
;; some transient UI state. Nothing in the app-db gets saved to storage and
;; much of it will get cleared every time the user switches to a different
;; page.
;;

(defonce db-schema (atom {:src {}}))


(defn register-app-db-key!
  "Registers the malli schema for a new top-level app-db key."
  ([db-key schema]
   (register-app-db-key! db-key {} schema))
  ([db-key opts schema]
   (swap! db-schema #(-> %
                         (assoc-in [:src db-key] [db-key opts schema])
                         (dissoc :schema)))))


(defn- src->schema
  [src]
  (m/schema (into [:map] (vals src))))


(defn- compiled-db-schema
  []
  (or (:schema @db-schema)
      (-> (swap! db-schema (fn [{:keys [src] :as value}]
                            (assoc value :schema (src->schema src))))
          :schema)))


(defn- db-errors
  [db]
  (-> (m/explain (compiled-db-schema) db)
      (me/humanize)))


(defn- validate-app-db
  [db _event]
  (when-some [errors (db-errors db)]
    (js/console.warn errors)))


;;
;; Initialization
;;

(defn init
  []
  (let [conn (or (create-conn-from-storage)
                 (ds/create-conn ds-schema))]
    (rp/connect! conn)

    ;; Queue auto-save after every DataScript transaction.
    (ds/listen! conn mark-dirty)

    (when ^boolean goog.DEBUG
      ;; Install validation hooks for development.
      (ds/listen! conn validate-tx-report)
      (rf/reg-global-interceptor (rf/after validate-app-db))

      ;; Report all DataScript changes to portal.
      (ds/listen! conn tap-datoms))))
