(ns refactory.app.ui.recipes
  "Recipe UI."
  (:require [clojure.string :as str]
            [re-frame.core :as rf]
            [refactory.app.game :as game]
            [refactory.app.ui.forms :as forms]
            [refactory.app.ui.items :as items]
            [refactory.app.ui.modal :as modal]
            [refactory.app.util :refer [forall per-minute]]))



(defn recipe-io
  "A visual representation of a recipe, with badged items showing inputs and
  outputs. Options:

    multiple: For the item badges, calculate multiple builders or iterations.
      Defaults to 1.
    per-minute?: If true, calculate per-minute figures for the badges.
    info? If true, add an icon to open a modal with more details.
    "
  ([recipe-id]
   (recipe-io recipe-id {}))
  ([recipe-id {:keys [multiple per-minute? info?]
               :or {multiple 1, per-minute? false, info? false}}]
   (let [recipe (game/id->recipe recipe-id)
         factor (if per-minute?
                  (per-minute multiple (:duration recipe))
                  multiple)]
     [:div.recipe-io
      (forall [{:keys [item-id amount]} (:input recipe)]
        ^{:key item-id} [items/item-io item-id (* amount factor) {:rate? per-minute?}])
      [:span.icon.has-text-black [:i.bi-caret-right-fill]]
      (forall [{:keys [item-id amount]} (:output recipe)]
        ^{:key item-id} [items/item-io item-id (* amount factor) {:rate? per-minute?}])
      (when info?
        [:button.button.is-white.is-small {:on-click #(rf/dispatch [::modal/show ::details {:recipe-id recipe-id}])}
         [:span.icon [:i.bi-info-circle]]])])))


;;
;; Recipe chooser
;;
;; This is a modal with the list of available recipes and a search box.
;;

;; Opens the recipe chooser modal. The caller provides dispatch vectors to get
;; the result: on-success (with the recipe-id appended) if a recipe is chosen,
;; on-cancel if the modal is dismissed.
(rf/reg-event-fx
  ::show-chooser
  (fn [{:keys [db]} [_ {:keys [search-term per-minute? on-success on-cancel]
                        :or {search-term "", per-minute? false}}]]
    (let [search-term (str/lower-case search-term)]
      {:db (assoc db ::chooser {:search-term search-term
                                :per-minute? per-minute?
                                :on-success on-success
                                :on-cancel on-cancel})
       :fx [[:dispatch [::modal/show ::chooser {::modal/close? false}]]]})))


;; Closes the chooser, optionally with a result. recipe-id is the selected
;; recipe or nil if the modal was closed.
(rf/reg-event-fx
  ::finish-chooser
  (fn [{:keys [db]} [_ recipe-id]]
    (let [{:keys [on-success on-cancel]} (::chooser db)]
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


(rf/reg-event-db
  ::set-search-term
  (fn [db [_ term]]
    (assoc-in db [::chooser :search-term] (str/lower-case (or term "")))))


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
  ::chooser-search-term
  :<- [::chooser-state]
  (fn [state _]
    (:search-term state)))


(rf/reg-sub
  ::chooser-per-minute?
  :<- [::chooser-state]
  (fn [state _]
    (:per-minute? state)))


(rf/reg-sub
  ::chooser-recipe-ids
  :<- [::game/unlocked-recipe-ids]
  :<- [::chooser-search-term]
  (fn [[recipe-ids term] _]
    (letfn [(recipe-matches? [recipe]
              (some #(str/includes? % term) (:search-terms recipe)))]
      (cond->> (sort-by game/recipe-sort-key recipe-ids)
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
  (let [per-minute? @(rf/subscribe [::chooser-per-minute?])]
    [:div.is-flex.is-align-content-center.mb-2
      [:a.is-flex-grow-1 {:on-click #(rf/dispatch [::chooser-expand-recipe recipe-id])}
       (recipe-io recipe-id {:per-minute? per-minute?})]
      [:a.is-flex-grow-0.ml-5.has-text-black
       [:span.icon.is-large {:on-click #(rf/dispatch [::finish-chooser recipe-id])}
        [:i.bi-plus-circle]]]]))


(defn- io-details
  "Details of either inputs or outputs of a recipe."
  [components duration]
  [:div.block.is-flex.is-flex-wrap-wrap
   (forall [{:keys [item-id amount]} components]
     (let [item (game/id->item item-id)]
       ^{:key item-id}
       [:div.mr-6.mb-4.is-inline-flex.is-align-content-center
        (items/item-io item-id amount)
        [:span.ml-3
         amount " " (:display item) [:br]
         (items/format-amount (per-minute amount duration)) "/min"]]))])


(defn recipe-details
  [recipe-id]
  (let [{:keys [input output duration builder-id]} (game/id->recipe recipe-id)]
    [:<>
     [:p.is-size-5.mb-2 "Inputs"]
     (io-details input duration)

     [:p.is-size-5.mb-2 "Outputs"]
     (io-details output duration)

     [:p.is-size-5 "Builder"]
     [:span (:display (game/id->builder builder-id)) ", " duration "s"]]))


(defn chooser-recipe-expanded
  [recipe-id]
  (let [{:keys [display alternate]} (game/id->recipe recipe-id)]
    [:div.box
     [:div.level.is-mobile
      [:div.level-left
       [:p.is-size-4 display (when alternate " (alt)")]]
      [:div.level-right
       [:button.delete {:on-click #(rf/dispatch [::chooser-expand-recipe nil])}]]]

     [recipe-details recipe-id]

     [:div.is-flex.is-justify-content-flex-end
      [:button.button.is-success {:on-click #(rf/dispatch [::finish-chooser recipe-id])}
        [:span.icon [:i.bi-plus-circle]]
        [:span "Add"]]]]))


(defmethod modal/content ::chooser
  []
  (let [recipe-ids @(rf/subscribe [::chooser-recipe-ids])
        expanded-id @(rf/subscribe [::chooser-expanded-recipe])
        per-minute? @(rf/subscribe [::chooser-per-minute?])]
    [:div.modal-card
     [:header.modal-card-head
      [:p.modal-card-title "Add a recipe"]
      [:button.delete {:on-click #(rf/dispatch [::finish-chooser nil])}]]
     [:section.modal-card-body
      [forms/search-field {:placeholder "Search by name or output"
                           :auto-focus? true
                           :on-update [::set-search-term]}]
      [:hr.hr]
      [:div.content
       [:p.help
        (when per-minute? "Showing units per minute. ")
        "Click on a recipe for more details or add it with "
        [:span.icon.is-small [:i.bi-plus-circle]]
        "."]]
      [:div.is-flex.is-flex-direction-column
       (forall [recipe-id recipe-ids]
         (if (= recipe-id expanded-id)
           ^{:key recipe-id} [chooser-recipe-expanded recipe-id]
           ^{:key recipe-id} [chooser-recipe-brief recipe-id]))]]]))


(defmethod modal/content ::details
  [{:keys [recipe-id]}]
  (let [{:keys [display alternate]} (game/id->recipe recipe-id)]
    [:div.modal-card
     [:header.modal-card-head
      [:p.modal-card-title display (when alternate " (alt)")]
      [:button.delete {:on-click #(rf/dispatch [::modal/hide ::details])}]]
     [:section.modal-card-body
      [recipe-details recipe-id]]]))
