(ns refactory.app.ui.items
  (:require [clojure.string :as str]
            [re-frame.core :as rf]
            [refactory.app.game :as game]
            [refactory.app.ui.forms :as forms]
            [refactory.app.ui.modal :as modal]
            [refactory.app.ui.recipes :as recipes]
            [refactory.app.util :refer [forall]]))


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
          [recipes/item-icon item-id {:class "is-large"}]])]]]))
