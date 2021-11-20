(ns refactory.app.db
  (:require [clojure.spec.alpha :as s]
            [datascript.core :as ds]
            [expound.alpha :as expound]
            [goog.functions :refer [debounce]]
            [posh.reagent]
            [re-frame.core :as rf]
            [re-frame.db]
            [re-posh.core :as rp]
            [re-posh.db]
            [taoensso.encore :refer [as-?bool as-?int as-?kw as-?nblank-trim
                                     as-?nat-int]]))


(when ^boolean goog.DEBUG
  (s/def ::dirty? boolean?))


;;
;; DataScript
;;
;; DataScript holds all persistent application data. When we save our state,
;; this is what we're saving.
;;

;; Our full DataScript schema is here. This may include some of our own keys:
;;
;;   :app/kind: An optional type identifier for decoding form inputs.
;;   :app/default: An optional default value for forms, queries, etc.
;;
(def ds-schema
  {;; Each page gets a record to store any page-global state.
   :page/id {:db/unique :db.unique/identity
             :app/kind :keyword}

   ;; For example, [:page/id :factories] will hold the currently selected
   ;; factory.
   :factories/selected {:db/valueType :db.type/ref
                        :app/kind :int}

   ;; Factories and their components (refactory.app.pages.factories).
   :factory/title {:db/index true
                   :app/kind :string
                   :app/default "New Factory"}
   :factory/mode {:app/kind :keyword
                  :app/default :continuous}
   :factory/jobs {:db/valueType :db.type/ref
                  :db/cardinality :db.cardinality/many
                  :db/isComponent true}

   :job/recipe-id {}
   :job/disabled? {:app/kind :boolean}
   ;; Number of iterations (for :fixed mode)
   :job/count {:app/kind :int
               :app/default 1}
   ;; Distinct builders (for :continuous mode)
   :job/instances {:db/valueType :db.type/ref
                   :db/cardinality :db.cardinality/many
                   :db/isComponent true}

   :instance/overdrive {:app/kind :int
                        :app/default 100}})


(defn ds
  "Convenience wrapper to the DataScript db, mainly for the REPL."
  []
  @@re-posh.db/store)


(defn attr->kind
  [attr]
  (get-in ds-schema [attr :app/kind]))


(defn attr->default
  [attr]
  (get-in ds-schema [attr :app/default]))


(defmulti decode-kind
  "Decodes a value according to :app/kind.

  This is mostly used to decode strings from form submissions, but can also be
  useful for validation. Implementations must be idempotent."
  (fn [_value kind] kind))

(defmethod decode-kind :default
  [value _]
  value)

(defmethod decode-kind :keyword
  [value _]
  (as-?kw value))

(defmethod decode-kind :int
  [value _]
  (as-?int value))

(defmethod decode-kind :boolean
  [value _]
  (as-?bool (as-?bool value)))


(defmulti decode-attr
  "Decodes a value according to attribute name.

  This can apply additional attribute-specific decoding on top of decode-kind.
  It should only be given values of the correct type and should also be
  idempotent. This is mainly used for rejecting values that don't meet more
  specific validation criteria (by returning nil)."
  (fn [_value attr] attr))

(defmethod decode-attr :default
  [value _]
  value)

(defmethod decode-attr :factory/title
  [value _]
  (as-?nblank-trim value))

(defmethod decode-attr :factory/mode
  [value _]
  (#{:continuous :fixed} value))

(defmethod decode-attr :job/count
  [value _]
  (as-?nat-int value))

(defmethod decode-attr :instance/overdrive
  [value _]
  (when (<= 0 value 250)
    value))


(defn decode-value
  "Attempts to decode the string representation of a value.

  If decoding fails, this will return nil or optionally substitute the default
  value (if any)."
  ([attr value]
   (decode-value attr value {}))
  ([attr value {:keys [default?]}]
   (let [kind (attr->kind attr)]
     (cond-> value
       kind     (some-> (decode-kind kind))
       attr     (some-> (decode-attr attr))
       default? (or (attr->default attr))))))


(defn- validate-tx-report
  "For development only: warns of invalid values inserted into DataScript."
  [{:keys [tx-data]}]
  (if (empty? tx-data)
    (js/console.warn "Transaction affected no datoms.")
    (doseq [[eid attr value _time added?] tx-data]
      (when added?
        (let [decoded (decode-value attr value)]
          (when-not (= decoded value)
            (js/console.warn (pr-str [eid attr value]) "did not decode to itself:" (pr-str decoded))))))))


(defn- tap-datoms
  "For development only: report DataScript activity."
  [{:keys [tx-data]}]
  (doseq [datom tx-data]
    (tap> (vec datom))))


;;
;; Persistence
;;
;; On save, we use DataScript's serialization to store the entire database,
;; including the schema and indexes. On load, we deserialize the database and
;; then extract the datoms and combine them with the latest schema. It's a kind
;; of naive migration for backwards-compatible schema changes.
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


(when ^boolean goog.DEBUG
  (defn- validate-app-db
    [db _event]
    (when-not (s/valid? (s/keys) db)
      (js/console.warn (expound/expound-str (s/keys) db)))))


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
