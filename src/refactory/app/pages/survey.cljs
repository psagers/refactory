(ns refactory.app.pages.survey
  (:require [clojure.set :as set]
            [datascript.core :as ds]
            [reagent.core :as r]
            [reagent.ratom :as ratom]
            [re-frame.core :as rf]
            [re-posh.core :as rp]
            [refactory.app.db :as db]
            [refactory.app.game :as game]
            [refactory.app.ui.items :as items]
            [refactory.app.ui.recipes :as recipes]
            [refactory.app.util :refer [forall]]))


(def default-exclusivity (db/attr->default :survey/exclusivity))


(defmethod db/ds-migration ::_
  [_ ds]
  (let [{:survey/keys [exclusivity] :as page} (ds/entity ds [:page/id :survey])]
    [(when-not page
       [:db/add -1 :page/id :survey])
     (when-not exclusivity
       [:db/add -1 :survey/exclusivity default-exclusivity])]))


;;
;; Events
;;

(rp/reg-event-ds
  ::set-exclusivity
  (fn [_ [_ value]]
    (when-some [exclusivity (db/decode-value :survey/exclusivity value)]
      [[:db/add [:page/id :survey] :survey/exclusivity exclusivity]])))


(rp/reg-event-ds
  ::add-ingredient
  (fn [_ [_ item-id]]
    [[:db/add [:page/id :survey] :survey/item-ids item-id]]))


(rp/reg-event-ds
  ::remove-ingredient
  (fn [_ [_ item-id]]
    [[:db/retract [:page/id :survey] :survey/item-ids item-id]]))


;;
;; Subscriptions
;;

(rp/reg-query-sub
  ::exclusivity
  '[:find ?exclusivity .
    :where [?e :page/id :survey]
           [?e :survey/exclusivity ?exclusivity]])


(rp/reg-query-sub
  ::item-ids
  '[:find [?item-id ...]
    :where [?e :page/id :survey]
           [?e :survey/item-ids ?item-id]])


(rf/reg-sub
  ::at-most-recipe-ids
  :<- [::game/unlocked-recipe-ids]
  :<- [::item-ids]
  (fn [[unlocked-recipe-ids item-ids] _]
    (loop [recipe-id-pool (set unlocked-recipe-ids)
           item-ids (set item-ids)
           recipe-ids #{}]
      (let [new-recipe-ids (set/select (partial game/can-build-with? item-ids)
                                       recipe-id-pool)]
        (if (empty? new-recipe-ids)
          (sort-by game/recipe-sort-key recipe-ids)
          (recur (set/difference recipe-id-pool new-recipe-ids)
                 (set/union item-ids (game/recipe-ids->output-ids new-recipe-ids))
                 (set/union recipe-ids new-recipe-ids)))))))


(rf/reg-sub
  ::at-least-recipe-ids
  :<- [::game/unlocked-recipe-ids]
  :<- [::item-ids]
  (fn [[unlocked-recipe-ids item-ids] _]
    (->> (apply set/intersection
                (map (partial game/item-id->deep-recipe-ids unlocked-recipe-ids) item-ids))
         (sort-by game/recipe-sort-key))))


(rf/reg-sub-raw
  ::buildable-recipe-ids
  (fn [_ _]
    (ratom/reaction
      (case @(rf/subscribe [::exclusivity])
        :at-most @(rf/subscribe [::at-most-recipe-ids])
        :at-least @(rf/subscribe [::at-least-recipe-ids])
        []))))


;;
;; Components
;;

(defn- options-form
  []
  (r/with-let [exclusivity-sub (rf/subscribe [::exclusivity])]
    (let [exclusivity @exclusivity-sub]
      [:div
       [:div.field
        [:div.control
         [:div.select
          [:select {:value (name exclusivity)
                    :on-change #(rf/dispatch [::set-exclusivity (-> % .-target .-value)])}
           [:option {:value "at-most"} "Only these ingredients"]
           [:option {:value "at-least"} "All of these ingredients"]]]]]
       [:div.field.has-text-centered]])))


(defn- ingredients-table
  []
  (r/with-let [item-ids-sub (rf/subscribe [::item-ids])]
    (let [item-ids @item-ids-sub]
      [:table.table
       [:thead>tr>th {:colSpan 2}]
       [:tbody
        (forall [item-id item-ids]
          ^{:key item-id}
          [:tr
           [:td
            [:div.is-flex.is-align-items-center
             [items/item-icon item-id]
             [:span.ml-2 (:display (game/id->item item-id))]]]
           [:td
            [:button.delete {:on-click #(rf/dispatch [::remove-ingredient item-id])}]]])]
       [:tfoot>tr>th {:colSpan 2}]])))


(defn- results-table
  []
  (let [recipe-ids @(rf/subscribe [::buildable-recipe-ids])]
    [:table.table.is-fullwidth
     [:thead
      [:tr
       [:th]]]
     [:tbody
      (forall [recipe-id recipe-ids]
        ^{:key recipe-id}
        [:tr
         [:td [recipes/recipe-io recipe-id {:per-minute? true
                                            :info? true}]]])]
     [:tfoot
      [:tr
       [:th]]]]))


(defn root
  []
  [:div.columns
   [:div.column.is-narrow
    [options-form]
    [ingredients-table]
    [:button.button.is-primary {:on-click #(rf/dispatch [::items/show-chooser {:on-success [::add-ingredient]}])}
     "Add an ingredient"]]
   [:div.column.ml-6
    [results-table]]])
