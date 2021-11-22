(ns refactory.app.ui.items
  (:require [clojure.string :as str]
            [re-frame.core :as rf]
            [reagent.core :as r]
            [refactory.app.game :as game]
            [refactory.app.ui.modal :as modal]
            [refactory.app.ui.recipes :as recipes]
            [refactory.app.util :refer [forall]])
  (:import [goog.async Debouncer]))


;;
;; Recipe chooser
;;
;; This is a modal with the list of available ingredients and a search box.
;;

(declare fire-search)


;; Opens the ingredient chooser modal. The caller provides dispatch vectors to
;; get the result: on-success (with the item-id appended) if an item is chosen,
;; on-cancel if the modal is dismissed.
(rf/reg-event-fx
  ::show-chooser
  (fn [{:keys [db]} [_ {:keys [search-term on-success on-cancel]}]]
    (let [search-term (str/lower-case (or search-term ""))]
      {:db (assoc db ::chooser {:debouncer (Debouncer. fire-search 350 nil)
                                ;; The actual text in the search box
                                :search-text search-term
                                ;; The active (debounced) search term
                                :search-term search-term
                                :on-success on-success
                                :on-cancel on-cancel})
       :fx [[:dispatch [::modal/show ::chooser {::modal/close? false}]]]})))


;; Closes the chooser, optionally with a result. item-id is the selected item
;; or nil if the modal was closed.
(rf/reg-event-fx
  ::finish-chooser
  (fn [{:keys [db]} [_ item-id]]
    (let [{:keys [debouncer on-success on-cancel]} (::chooser db)]
      (when debouncer
        (.stop debouncer))

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


;; Copy :search-text to :search-term to update the filter. The filtering is a
;; bit expensive, so we debounce this step.
(rf/reg-event-fx
  ::update-search-term
  (fn [{:keys [db]} _]
    (when-some [{:keys [search-text]} (::chooser db)]
      {:db (assoc-in db [::chooser :search-term] (or search-text ""))})))


(defn- fire-search
  "Called by the debouncer."
  []
  (rf/dispatch [::update-search-term]))


(rf/reg-fx
  ::debounce-search-text
  (fn [^Debouncer debouncer]
    (.fire debouncer)))


(rf/reg-event-fx
  ::set-search-text
  (fn [{:keys [db]} [_ text sync?]]
    (when-some [{:keys [debouncer]} (::chooser db)]
      (let [text (or (some-> text str/lower-case) "")]
        {:db (assoc-in db [::chooser :search-text] text)
         :fx [(if sync?
                [:dispatch [::update-search-term]]
                [::debounce-search-text debouncer])]}))))


(rf/reg-sub
  ::chooser-state
  (fn [db _]
    (::chooser db)))


(rf/reg-sub
  ::chooser-search-text
  :<- [::chooser-state]
  (fn [state _]
    (:search-text state)))


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
  (r/with-let [search-text-sub (rf/subscribe [::chooser-search-text])
               item-ids-sub (rf/subscribe [::chooser-item-ids])]
    (let [search-text @search-text-sub
          item-ids @item-ids-sub]
      [:div.modal-card
       [:header.modal-card-head
        [:p.modal-card-title "Add an ingredient"]
        [:button.delete {:on-click #(rf/dispatch [::finish-chooser nil])}]]
       [:section.modal-card-body
        [:div
         [:div.control.has-icons-right
          [:input.input.is-rounded {:type "text"
                                    :placeholder "Search by name"
                                    :autoFocus true
                                    :value search-text
                                    :on-change #(rf/dispatch-sync [::set-search-text (-> % .-target .-value)])}]
          [:span.icon.is-right
           [:button.delete {:disabled (empty? search-text)
                            :on-click #(rf/dispatch [::set-search-text "" true])}]]]]
        [:hr.hr]
        [:div.is-flex.is-justify-content-space-between.is-flex-wrap-wrap
         (forall [item-id item-ids]
           ^{:key item-id}
           [:button.button.is-large.m-1 {:on-click #(rf/dispatch [::finish-chooser item-id])}
            [recipes/item-icon item-id {:class "is-large"}]])]]])))
