(ns refactory.app.db
  (:require [datascript.core :as ds]
            [goog.functions :refer [debounce]]
            [malli.core :as m]
            [malli.error :as me]
            [posh.reagent]
            [re-frame.core :as rf]
            [re-posh.core :as rp]
            [re-posh.db]))


;;
;; DataScript
;;

(defonce ds-schema (atom {}))


(defn ds []
  @@re-posh.db/store)


(defn register-ds-schema!
  "Registers a (partial) DataScript schema.

  Namespaces should call this at load time to register their attributes. The
  argument will be merged with other registrations to form the full schema
  passed to datascript.core/create-conn."
  [schema]
  (when (nil? @re-posh.db/store)
    (when-some [conflicts (not-empty (filter (partial contains? @ds-schema) (keys schema)))]
      (js/console.warn "Overwriting schema attributes:" (pr-str conflicts))))

  (swap! ds-schema merge schema))


(defn attr->malli-schema
  [attr]
  (get-in @ds-schema [attr :valid/malli]))


(defn- validate-tx-report
  [{:keys [tx-data]}]
  (doseq [[eid attr value _timestamp added?] tx-data]
    (when added?
      (when-some [schema (attr->malli-schema attr)]
        (when-not (m/validate schema value)
          (doseq [msg (me/humanize (m/explain schema value))]
            (js/console.warn (pr-str [eid attr value]) "- value" msg)))))))


(defn- tap-datoms
  [{:keys [tx-data]}]
  (doseq [datom tx-data]
    (tap> (vec datom))))


(def STORAGE-KEY "autosave")


(defn- save-to-storage!
  []
  (->> (ds)
       (ds/serializable)
       (js/JSON.stringify)
       (.setItem js/window.localStorage STORAGE-KEY)))


(defn- create-conn-from-storage
  []
  (when-some [data (. js/window.localStorage getItem STORAGE-KEY)]
    (-> data
        (js/JSON.parse)
        (ds/from-serializable)
        (ds/conn-from-db))))


(rf/reg-fx
  ::save
  (fn [_]
    (save-to-storage!)))


(def save-soon (debounce #(rf/dispatch [::save]) 1000))


(rf/reg-fx
  ::save-soon
  (fn [_]
    (save-soon)))


(rf/reg-event-fx
  ::mark-dirty
  (fn [{:keys [db]} _]
    {:db (assoc db ::dirty? true)
     :fx [[::save-soon]]}))


(rf/reg-event-fx
  ::save
  (fn [{:keys [db]} _]
    {:db (dissoc db ::dirty?)
     :fx [[::save]]}))


(defn- mark-dirty
  []
  (rf/dispatch [::mark-dirty]))


(rf/reg-sub
  ::dirty?
  (fn [db _]
    (::dirty? db)))


(defn init
  []
  (let [conn (or (create-conn-from-storage)
                 (ds/create-conn @ds-schema))]
    (rp/connect! conn)

    (ds/listen! conn mark-dirty)

    (when ^boolean goog.DEBUG
      (ds/listen! conn validate-tx-report)
      (ds/listen! conn tap-datoms))

    conn))


(defn tap-ds
  []
  (tap> (mapv (comp ds/touch (partial ds/entity (ds)))
              (ds/q '[:find [?e ...] :where [?e]] (ds)))))


(comment
  (tap-ds))


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

(defonce db-schema (atom {:src {}}))


(defn register-db-entry!
  "Registers the malli schema for a new top-level db entry."
  ([db-key schema]
   (register-db-entry! db-key {} schema))
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


(defn db-errors
  [db]
  (-> (m/explain (compiled-db-schema) db)
      (me/humanize)))


(comment
  (m/ast [:map [::key {:min 1} :int]])
  (m/validate (m/from-ast {:type :map
                           :keys {::key (m/ast [:int {:min 1}])}})
              {}))
