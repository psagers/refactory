(ns refactory.app.db
  (:require [malli.core :as m]
            [malli.error :as me]))


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
