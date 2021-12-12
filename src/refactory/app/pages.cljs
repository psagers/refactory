(ns refactory.app.pages
  (:require [re-frame.core :as rf]))


(defmulti page-config
  "Configuration for each page. Should return a map with any of our supported
  keys:

    :enter - A re-frame event to dispatch before switching to the page.
    :leave - A re-frame event to dispatch before switching away from a page.
  "
  identity)


(defmethod page-config :default
  [_]
  {})


;;
;; Active page
;;

(rf/reg-event-db
  ::set-page
  (fn [db [_ page]]
    (assoc db ::page page)))


(rf/reg-sub
  ::page
  (fn [db _]
    (::page db :blank)))


(rf/reg-event-fx
  ::switch-to
  (fn [db [_ page opts]]
    (let [old (::page db)]
      {:fx [(when-some [leave (:leave (page-config old))]
              [:dispatch leave])
            (when-some [enter (:enter (page-config page))]
              [:dispatch (conj enter opts)])
            [:dispatch [::set-page page]]]})))
