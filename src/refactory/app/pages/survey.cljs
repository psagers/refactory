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
(def default-badge-mode (db/attr->default :survey/badge-mode))


(defmethod db/ds-migration ::_
  [_ ds]
  (let [{:survey/keys [exclusivity badge-mode] :as page} (ds/entity ds [:page/id :survey])]
    [(when-not page
       [:db/add -1 :page/id :survey])
     (when-not exclusivity
       [:db/add -1 :survey/exclusivity default-exclusivity])
     (when-not badge-mode
       [:db/add -1 :survey/badge-mode default-badge-mode])]))


;;
;; Events
;;

(defn- set-option-ds
  [attr value]
  (when-some [decoded (db/decode-value attr value)]
    [[:db/add [:page/id :survey] attr decoded]]))


(rp/reg-event-ds
  ::set-exclusivity
  (fn [_ [_ value]]
    (set-option-ds :survey/exclusivity value)))


(rp/reg-event-ds
  ::add-ingredient
  (fn [_ [_ item-id]]
    [[:db/add [:page/id :survey] :survey/item-ids item-id]]))


(rp/reg-event-ds
  ::remove-ingredient
  (fn [_ [_ item-id]]
    [[:db/retract [:page/id :survey] :survey/item-ids item-id]]))


(rp/reg-event-ds
  ::set-badge-mode
  (fn [_ [_ value]]
    (set-option-ds :survey/badge-mode value)))


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


(rp/reg-query-sub
  ::badge-mode
  '[:find ?mode .
    :where [?e :page/id :survey]
           [?e :survey/badge-mode ?mode]])


;; Finds all of the matching recipes in :at-most mode. Returns a map of
;; recipe-id to the number of build hops required to get there.
(rf/reg-sub
  ::at-most-recipe-ids
  :<- [::game/unlocked-recipe-ids]
  :<- [::item-ids]
  (fn [[unlocked-recipe-ids item-ids] _]
    (loop [recipe-id-pool (set unlocked-recipe-ids)
           item-ids (set item-ids)
           recipe-id->hop {}
           hops 1]
      (let [new-recipe-ids (set/select (partial game/can-build-with? item-ids)
                                       recipe-id-pool)]
        (if (empty? new-recipe-ids)
          recipe-id->hop
          (recur (set/difference recipe-id-pool new-recipe-ids)
                 (set/union item-ids (game/recipe-ids->output-ids new-recipe-ids))
                 (into recipe-id->hop (for [recipe-id new-recipe-ids] [recipe-id hops]))
                 (inc hops)))))))


;; Finds all of the matching recipes in :at-least mode. Returns a map of
;; recipe-id to the number of build hops required to get there.
(rf/reg-sub
  ::at-least-recipe-ids
  :<- [::game/unlocked-recipe-ids]
  :<- [::item-ids]
  (fn [[unlocked-recipe-ids item-ids] _]
    (let [;; seq of recipe-id->hops
          results (map (partial game/item-id->deep-recipe-ids unlocked-recipe-ids) item-ids)
          ;; set of recipe-id
          recipe-ids (apply set/intersection (map (comp set keys) results))]

      (apply merge-with max (map #(select-keys % recipe-ids) results)))))


;; Selects between ::at-most-recipe-ids and ::at-least-recipe-ids and applies
;; final transformations. Returns a sequence of [recipe-id hops] pairs.
(rf/reg-sub-raw
  ::buildable-recipe-ids-by-hop
  (fn [_ _]
    (ratom/reaction
      (->> (case @(rf/subscribe [::exclusivity])
             :at-most @(rf/subscribe [::at-most-recipe-ids])
             :at-least @(rf/subscribe [::at-least-recipe-ids])
             [])
           (sort-by (fn [[recipe-id hops]]
                      [hops (game/recipe-sort-key recipe-id)]))))))


;;
;; Components
;;

(defn- options-form
  []
  (r/with-let [exclusivity-sub (rf/subscribe [::exclusivity])]
    (let [exclusivity @exclusivity-sub]
      [:div
       [:div.field
        [:label.label "Find recipes with:"]
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
      (when (seq item-ids)
        [:table.table.is-fullwidth
         [:thead>tr>th {:colSpan 2}]
         [:tbody
          (forall [item-id item-ids]
            ^{:key item-id}
            [:tr
             [:td
              [:div.is-flex.is-align-items-center
               [items/item-icon item-id]
               [:span.ml-2 (:display (game/id->item item-id))]]]
             [:td.has-text-right
              [:button.delete {:on-click #(rf/dispatch [::remove-ingredient item-id])}]]])]
         [:tfoot>tr>th {:colSpan 2}]]))))


(defn- results-table
  []
  (let [results @(rf/subscribe [::buildable-recipe-ids-by-hop])
        badge-mode @(rf/subscribe [::badge-mode])]
    (when (seq results)
      [:table.table.is-fullwidth
       [:thead
        [:tr
         [:th
          [:span.mr-5 "Recipe"]
          [:div.select.is-small
           [:select {:value (name (or badge-mode default-badge-mode))
                     :on-change #(rf/dispatch [::set-badge-mode (-> % .-target .-value)])}
            [:option {:value "continuous"} "Per minute"]
            [:option {:value "fixed"} "Per build"]]]]
         [:th "Hops"]]]
       [:tbody
        (forall [[recipe-id hops] results]
          ^{:key recipe-id}
          [:tr
           [:td [recipes/recipe-io recipe-id {:per-minute? (= badge-mode :continuous)
                                              :info? true}]]
           [:td hops]])]
       [:tfoot
        [:tr
         [:th]
         [:th]]]])))


(defn root
  []
  [:div.columns
   [:div.column.is-narrow
    [:div.box
     [options-form]
     [ingredients-table]
     [:button.button.is-primary {:on-click #(rf/dispatch [::items/show-chooser {:on-success [::add-ingredient]}])}
      "Add an ingredient"]]]
   [:div.column
    [results-table]]])
