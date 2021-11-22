(ns refactory.app.ui.recipes
  "Recipe UI."
  (:require [clojure.string :as str]
            [re-frame.core :as rf]
            [refactory.app.game :as game]
            [refactory.app.ui.modal :as modal]
            [refactory.app.ui.forms :as forms]
            [refactory.app.util :refer [forall per-minute]]))


;;
;; Generic recipe UI
;;

(def amount-formatter (js/Intl.NumberFormat. js/undefined
                                             #js {:maximumFractionDigits 1
                                                  :useGrouping false}))

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
    (str (.format amount-formatter (/ amount scale)) suffix)))


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
   (item-io item-id nil))
  ([item-id amount]
   (let [item (game/id->item item-id)]
     [:div.item-io
      [:div.item-io-content
       [:img {:src (item->icon-path item)
              :alt (:display item)
              :title (:display item)
              :loading "lazy"}]
       (when amount
         [:span.amount (amount->badge amount)])]])))


(defn recipe-io
  "A visual representation of a recipe."
  ([recipe-id]
   (recipe-io recipe-id 1))
  ([recipe-id factor]
   (let [recipe (game/id->recipe recipe-id)]
     [:div.recipe-io
      (forall [{:keys [item-id amount]} (:input recipe)]
        ^{:key item-id} [item-io item-id (* amount factor)])
      [:span.icon.has-text-black [:i.bi-caret-right-fill]]
      (forall [{:keys [item-id amount]} (:output recipe)]
        ^{:key item-id} [item-io item-id (* amount factor)])])))


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
  (fn [{:keys [db]} [_ {:keys [search-term on-success on-cancel]}]]
    (let [search-term (str/lower-case (or search-term ""))]
      {:db (assoc db ::chooser {:search-term search-term
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
  [:div.is-flex.is-align-content-center.mb-2
    [:a.is-flex-grow-1 {:on-click #(rf/dispatch [::chooser-expand-recipe recipe-id])}
     (recipe-io recipe-id)]
    [:a.is-flex-grow-0.ml-5.has-text-black
     [:span.icon.is-large {:on-click #(rf/dispatch [::finish-chooser recipe-id])}
      [:i.bi-plus-circle]]]])


(defn- io-details
  "Details of either inputs or outputs of a recipe."
  [components duration]
  [:div.block.is-flex.is-flex-wrap-wrap
   (forall [{:keys [item-id amount]} components]
     (let [item (game/id->item item-id)]
       ^{:key item-id}
       [:div.mr-6.mb-4.is-inline-flex.is-align-content-center
        (item-io item-id amount)
        [:span.ml-3
         amount " " (:display item) [:br]
         (per-minute amount duration) "/min"]]))])


(defn recipe-details
  [recipe-id]
  (let [{:keys [input output duration]} (game/id->recipe recipe-id)]
    [:<>
     [:p.is-size-5.block "Inputs"]
     (io-details input duration)

     [:p.is-size-5.mt-5.block "Outputs"]
     (io-details output duration)]))


(defn chooser-recipe-expanded
  [recipe-id]
  (let [{:keys [display builder-id duration alternate]} (game/id->recipe recipe-id)]
    [:div.box
     [:div.level.is-mobile
      [:div.level-left
       [:p.is-size-4 display (when alternate " (alt)")]]
      [:div.level-right
       [:button.delete {:on-click #(rf/dispatch [::chooser-expand-recipe nil])}]]]

     [recipe-details recipe-id]

     [:p.is-size-5.block "Builder"]
     [:div.level
      [:div.level-left
       (:display (game/id->builder builder-id)) ", " duration "s"]
      [:div.level-right
       [:button.button.is-success {:on-click #(rf/dispatch [::finish-chooser recipe-id])}
         [:span.icon [:i.bi-plus-circle]]
         [:span "Add"]]]]]))


(defmethod modal/content ::chooser
  []
  (let [recipe-ids @(rf/subscribe [::chooser-recipe-ids])
        expanded-id @(rf/subscribe [::chooser-expanded-recipe])]
    [:div.modal-card
     [:header.modal-card-head
      [:p.modal-card-title "Add a recipe"]
      [:button.delete {:on-click #(rf/dispatch [::finish-chooser nil])}]]
     [:section.modal-card-body
      [forms/search-field {:on-update [::set-search-term]}]
      [:hr.hr]
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
