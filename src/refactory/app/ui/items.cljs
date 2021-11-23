(ns refactory.app.ui.items
  (:require [clojure.string :as str]
            [re-frame.core :as rf]
            [refactory.app.game :as game]
            [refactory.app.ui.forms :as forms]
            [refactory.app.ui.modal :as modal]
            [refactory.app.util :refer [forall]]))

;;
;; General UI
;;

(def amount-formatter (js/Intl.NumberFormat. js/undefined
                                             #js {:maximumFractionDigits 1
                                                  :useGrouping false}))


(defn format-amount
  [amount]
  (.format amount-formatter amount))


(def ^:private amount-scales
  [[0 ""]
   [1e3 "k"]
   [1e6 "m"]
   [1e9 "b"]
   [1e12 "t"]
   [1e15 "q"]])


(defn amount->badge
  "Renders an amount to a string suitable for an icon badge."
  [amount]
  (let [[scale _ :as bracket] (last (take-while (fn [[scale _]]
                                                  (<= scale (Math/abs amount)))
                                                amount-scales))
        [scale suffix] (cond
                         (nil? bracket) (peek amount-scales)
                         (zero? scale) (assoc bracket 0 1)
                         :else bracket)]
    (str (format-amount (/ amount scale)) suffix)))


(comment
  (map amount->badge [0 1 500 999 1000 -1500 1999 -20050 999999 1254000]))


(defn- item->icon-path
  [item]
  (str "img/" (:icon item)))


(defn item-icon
  ([item-id]
   (item-icon item-id {}))
  ([item-id attrs]
   (let [item (game/id->item item-id)]
     [:div.icon attrs
      [:img {:src (item->icon-path item)
             :alt (:display item)
             :title (:display item)
             :loading "lazy"}]])))


(defn item-io
  "A component indicating an item along with an optional amount."
  ([item-id]
   (item-io item-id nil {}))
  ([item-id amount]
   (item-io item-id amount {}))
  ([item-id amount {:keys [rate?]}]
   (let [item (game/id->item item-id)]
     [:div.item-io
      [:div.item-io-content
       [:img {:src (item->icon-path item)
              :alt (:display item)
              :title (:display item)
              :loading "lazy"}]
       (when amount
         (if rate?
           [:i.amount (amount->badge amount)]
           [:span.amount (amount->badge amount)]))]])))


;;
;; Item chooser
;;
;; This is a modal with the list of available ingredients and a search box.
;;

;; Opens the ingredient chooser modal. The caller provides dispatch vectors to
;; get the result: on-success (with the item-id appended) if an item is chosen,
;; on-cancel if the modal is dismissed.
(rf/reg-event-fx
  ::show-chooser
  (fn [{:keys [db]} [_ {:keys [search-term on-success on-cancel]}]]
    (let [search-term (str/lower-case (or search-term ""))]
      {:db (assoc db ::chooser {:search-term search-term
                                :on-success on-success
                                :on-cancel on-cancel})
       :fx [[:dispatch [::modal/show ::chooser {::modal/close? false}]]]})))


;; Closes the chooser, optionally with a result. item-id is the selected item
;; or nil if the modal was closed.
(rf/reg-event-fx
  ::finish-chooser
  (fn [{:keys [db]} [_ item-id]]
    (let [{:keys [on-success on-cancel]} (::chooser db)]
      {:fx [[:dispatch [::modal/hide ::chooser]]
            [:dispatch [::reset-chooser]]
            (cond
              (and (some? item-id) on-success)
              [:dispatch (conj on-success item-id)]

              (and (nil? item-id) on-cancel)
              [:dispatch on-cancel])]})))


(rf/reg-event-db
  ::reset-chooser
  (fn [db _]
    (dissoc db ::chooser)))


(rf/reg-event-db
  ::set-search-term
  (fn [db [_ term]]
    (assoc-in db [::chooser :search-term] (str/lower-case (or term "")))))


(rf/reg-sub
  ::chooser-state
  (fn [db _]
    (::chooser db)))


(rf/reg-sub
  ::chooser-search-term
  :<- [::chooser-state]
  (fn [state _]
    (:search-term state)))


(rf/reg-sub
  ::chooser-item-ids
  :<- [::game/unlocked-input-ids]
  :<- [::chooser-search-term]
  (fn [[item-ids term] _]
    (letfn [(item-matches? [item]
              (some #(str/includes? % term) (:search-terms item)))]
      (cond->> (sort-by game/item-sort-key item-ids)
        (not-empty term)
        (filter (comp item-matches? game/id->item))))))


(defmethod modal/content ::chooser
  []
  (let [item-ids @(rf/subscribe [::chooser-item-ids])]
    [:div.modal-card
     [:header.modal-card-head
      [:p.modal-card-title "Add an ingredient"]
      [:button.delete {:on-click #(rf/dispatch [::finish-chooser nil])}]]
     [:section.modal-card-body
      [forms/search-field {:placeholder "Search by name"
                           :auto-focus? true
                           :on-update [::set-search-term]}]
      [:hr.hr]
      [:div.is-flex.is-justify-content-space-between.is-flex-wrap-wrap
       (forall [item-id item-ids]
         ^{:key item-id}
         [:button.button.is-large.m-1 {:on-click #(rf/dispatch [::finish-chooser item-id])}
          (item-icon item-id {:class "is-large"})])]]]))
