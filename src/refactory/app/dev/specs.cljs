(ns refactory.app.dev.specs
  "A namespace to keep spec out of the release builds."
  (:require [clojure.spec.alpha :as s]
            [expound.alpha :as expound]
            [re-frame.core :as rf]
            [refactory.app :as app]
            [refactory.app.db :as db]))


(s/def ::app/page keyword?)
(s/def ::app/expanded-navbars (s/coll-of keyword?, :kind set?))

(s/def ::db/dirty? boolean?)


(defn- validate-app-db
  [db _event]
  (when-not (s/valid? (s/keys) db)
    (js/console.warn (expound/expound-str (s/keys) db))))


(rf/reg-global-interceptor (rf/after validate-app-db))
