(ns refactory.app.pages
  (:require [refactory.app.db :as db]))


(db/register-ds-schema!
  {:page/id {:db/unique :db.unique/identity
             :valid/malli :keyword}})


(defmulti config identity)


(defmethod config :default
  [_]
  {})
