(ns refactory.app.ui.recipes
  "Recipe UI."
  (:require [clojure.string :as str]
            [re-frame.core :as rf]
            [reagent.core :as r]
            [refactory.app.game :as game]
            [refactory.app.ui.modal :as modal]
            [refactory.app.util :refer [forall per-minute]])
  (:import [goog.async Debouncer]))


;;
;; Generic recipe UI
;;

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
             :title (:display item)}]])))


(defn item-io
  "A component indicating an item along with an optional amount."
  ([item-id]
   (item-io item-id nil))
  ([item-id amount]
   (let [item (game/id->item item-id)]
     [:div.item-io
      [:div.item-io-content
       [:img {:src (item->icon-path item)
              :alt (:display item)
              :title (:display item)}]
       (when amount
         [:span.amount amount])]])))


(defn recipe-io
  "A visual representation of a recipe."
  [recipe-id]
  (let [recipe (game/id->recipe recipe-id)]
    [:div.recipe-io
     (forall [{:keys [item-id amount]} (:input recipe)]
       ^{:key item-id} [item-io item-id amount])
     [:span.icon [:i.bi-caret-right-fill]]
     (forall [{:keys [item-id amount]} (:output recipe)]
       ^{:key item-id} [item-io item-id amount])]))


;;
;; Recipe chooser
;;
;; This is a modal with the list of available recipes and a search box.
;;

(declare fire-search)


;; Opens the recipe chooser modal. The caller provides dispatch vectors to get
;; the result: on-success (with the recipe-id appended) if a recipe is chosen,
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


;; Closes the chooser, optionally with a result. recipe-id is the selected
;; recipe or nil if the modal was closed.
(rf/reg-event-fx
  ::finish-chooser
  (fn [{:keys [db]} [_ recipe-id]]
    (let [{:keys [debouncer on-success on-cancel]} (::chooser db)]
      (when debouncer
        (.stop debouncer))

      {:fx [[:dispatch [::modal/hide ::chooser]]
            [:dispatch [::reset-chooser]]
            (cond
              (and (some? recipe-id) on-success)
              [:dispatch (conj on-success recipe-id)]

              (and (nil? recipe-id) on-cancel)
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
      (let [text (or text "")]
        {:db (assoc-in db [::chooser :search-text] text)
         :fx [(if sync?
                [:dispatch [::update-search-term]]
                [::debounce-search-text debouncer])]}))))


;; Marks a recipe as expanded in the chooser list. Pass nil to collapse any
;; currently expanded recipe.
(rf/reg-event-db
  ::chooser-expand-recipe
  (fn [db [_ recipe-id]]
    (assoc-in db [::chooser :expanded] recipe-id)))


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
  ::chooser-recipe-ids
  :<- [::chooser-search-term]
  (fn [term _]
    (letfn [(recipe-matches? [recipe]
              (some #(str/includes? % term) (:search-terms recipe)))]
      (cond->> (game/sorted-recipe-ids)
        (not-empty term)
        (filter (comp recipe-matches? game/id->recipe))))))


(rf/reg-sub
  ::chooser-expanded-recipe
  :<- [::chooser-state]
  (fn [state _]
    (:expanded state)))


(defn- chooser-recipe-brief
  "The one-line representation of a recipe in the chooser list."
  [recipe-id]
  [:div.is-flex.is-align-content-center.mb-2
    [:a.is-flex-grow-1 {:on-click #(rf/dispatch [::chooser-expand-recipe recipe-id])}
     (recipe-io recipe-id)]
    [:a.is-flex-grow-0.ml-5.has-text-black
     [:span.icon.is-large {:on-click #(rf/dispatch [::finish-chooser recipe-id])}
      [:i.bi-file-plus]]]])


(defn- io-details
  "Details of either inputs or outputs of a recipe."
  [components duration]
  [:div.is-flex.is-flex-wrap-wrap
   (forall [{:keys [item-id amount]} components]
     (let [item (game/id->item item-id)]
       ^{:key item-id}
       [:div.mr-6.mb-4.is-inline-flex.is-align-content-center
        (item-io item-id amount)
        [:span.ml-3
         amount " " (:display item) [:br]
         (per-minute amount duration) "/min"]]))])


(defn- chooser-recipe-expanded
  "The expanded representation of a recipe in the chooser list."
  [recipe-id]
  (let [{:keys [display input output builder-id duration alternate]} (game/id->recipe recipe-id)]
    [:div.box
     [:div.level.is-mobile
      [:div.level-left>p.is-size-4 display (when alternate " (alt)")]
      [:div.level-right>button.delete {:on-click #(rf/dispatch [::chooser-expand-recipe nil])}]]

     [:p.is-size-5.mb-4 "Inputs"]
     (io-details input duration)

     [:p.is-size-5.mt-5.mb-4 "Outputs"]
     (io-details output duration)

     [:p.is-size-5.mt-4.mb-4 "Builder"]
     [:div.level
      [:div.level-left
       (:display (game/id->builder builder-id)) ", " duration "s"]
      [:div.level-right
       [:button.button.is-success {:on-click #(rf/dispatch [::finish-chooser recipe-id])}
         [:span "Add"]
         [:span.icon.is-small [:i.bi-file-plus]]]]]]))


(defmethod modal/content ::chooser
  []
  (r/with-let [search-text-sub (rf/subscribe [::chooser-search-text])
               recipe-ids-sub (rf/subscribe [::chooser-recipe-ids])
               expanded-id-sub (rf/subscribe [::chooser-expanded-recipe])]
    (let [search-text @search-text-sub
          recipe-ids @recipe-ids-sub
          expanded-id @expanded-id-sub]
      [:div.modal-card
       [:header.modal-card-head
        [:p.modal-card-title "Add a recipe"]
        [:button.delete {:on-click #(rf/dispatch [::finish-chooser nil])}]]
       [:section.modal-card-body
        [:div
         [:div.control.has-icons-right
          [:input.input.is-rounded {:type "text"
                                    :placeholder "Search by name or output"
                                    :value search-text
                                    :on-change #(rf/dispatch-sync [::set-search-text (-> % .-target .-value)])}]
          [:span.icon.is-right
           [:button.delete {:disabled (empty? search-text)
                            :on-click #(rf/dispatch [::set-search-text "" true])}]]]]
        [:hr.hr]
        [:div.is-flex.is-flex-direction-column
         (forall [recipe-id recipe-ids]
           (if (= recipe-id expanded-id)
             ^{:key recipe-id} [chooser-recipe-expanded recipe-id]
             ^{:key recipe-id} [chooser-recipe-brief recipe-id]))]]])))
