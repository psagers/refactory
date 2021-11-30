(ns refactory.app.ui.items
  (:require [clojure.string :as str]
            [re-frame.core :as rf]
            [refactory.app.game :as game]
            [refactory.app.ui.forms :as forms]
            [refactory.app.ui.modal :as modal]
            [refactory.app.util :refer [cofx->fx forall]]))

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

(defn- toggle-item-fx
  [fx item-id]
  (update-in fx [:db ::chooser :selected]
             #(if (contains? % item-id)
                (disj % item-id)
                (conj % item-id))))


(defn- reset-chooser-fx
  [fx]
  (update fx :db dissoc ::chooser))


(defn- finish-chooser-fx
  [fx]
  (let [{:keys [multiple? selected on-success on-cancel]} (get-in fx [:db ::chooser])
        event (cond
                multiple?         (some-> on-success (conj selected))
                (empty? selected) on-cancel
                :else             (some-> on-success (conj (first selected))))]
    (-> fx
        (reset-chooser-fx)
        (update :fx into [[:dispatch [::modal/hide ::chooser]]
                          (when event [:dispatch event])]))))


;; Opens the ingredient chooser modal. The caller provides optional dispatch
;; vectors to get the result:
;;   (conj on-success item-ids) if items are chosen with multiple? true
;;   (conj on-success item-id)  if an item is chosen with multiple? false
;;   on-cancel if the modal is dismissed
(rf/reg-event-fx
  ::show-chooser
  (fn [{:keys [db]} [_ {:keys [multiple? initial search-term on-success on-cancel]}]]
    (let [search-term (str/lower-case (or search-term ""))]
      {:db (assoc db ::chooser {:multiple? multiple?
                                :search-term search-term
                                :initial (set initial)
                                :selected (set initial)
                                :on-success on-success
                                :on-cancel on-cancel})
       :fx [[:dispatch [::modal/show ::chooser {::modal/close? false}]]]})))


(rf/reg-event-fx
  ::item-selected
  (fn [cofx [_ item-id]]
    (let [multiple? (get-in cofx [:db ::chooser :multiple?])
          fx (cofx->fx cofx)]
      (cond-> (toggle-item-fx fx item-id)
        (not multiple?) (finish-chooser-fx)))))


(rf/reg-event-db
  ::reset-chooser
  (fn [db _]
    (update db ::chooser #(assoc % :selected (:initial %)))))


(rf/reg-event-fx
  ::cancel-chooser
  (fn [{:keys [db]} _]
    (let [{:keys [on-cancel]} (::chooser db)]
      {:fx [[:dispatch [::modal/hide ::chooser]]
            (when on-cancel [:dispatch on-cancel])]})))


(rf/reg-event-fx
  ::finish-chooser
  (fn [cofx _]
    (finish-chooser-fx (cofx->fx cofx))))


(rf/reg-event-db
  ::set-search-term
  (fn [db [_ term]]
    (assoc-in db [::chooser :search-term] (str/lower-case (or term "")))))


(rf/reg-sub
  ::chooser-state
  (fn [db _]
    (::chooser db)))


(rf/reg-sub
  ::chooser-multiple?
  :<- [::chooser-state]
  (fn [state _]
    (:multiple? state)))


(rf/reg-sub
  ::chooser-search-term
  :<- [::chooser-state]
  (fn [state _]
    (:search-term state)))


(rf/reg-sub
  ::chooser-selected
  :<- [::chooser-state]
  (fn [state _]
    (:selected state)))


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
  (let [item-ids @(rf/subscribe [::chooser-item-ids])
        multiple? @(rf/subscribe [::chooser-multiple?])
        item-id->selected? @(rf/subscribe [::chooser-selected])]
    [:div.modal-card
     [:header.modal-card-head
      [:p.modal-card-title "Add an ingredient"]
      [:button.delete {:on-click #(rf/dispatch [::cancel-chooser])}]]
     [:section.modal-card-body
      [forms/search-field {:placeholder "Search by name"
                           :auto-focus? true
                           :on-update [::set-search-term]}]
      [:hr.hr]
      [:div.is-flex.is-justify-content-flex-start.is-flex-wrap-wrap
       (forall [item-id item-ids]
         ^{:key item-id}
         [:button.button.is-large.m-1 {:class [(when (item-id->selected? item-id) "is-dark")]
                                       :on-click #(rf/dispatch [::item-selected item-id])}
          (item-icon item-id {:class "is-large"})])]]
     (when multiple?
       [:footer.modal-card-foot.is-justify-content-space-between
        [:div.is-flex.is-justify-content-flex-start
         [:button.button {:on-click #(rf/dispatch [::reset-chooser])}
          "Reset"]]
        [:div.is-flex.is-justify-content-flex-end
         [:button.button {:on-click #(rf/dispatch [::cancel-chooser])}
          "Cancel"]
         [:button.button.is-success {:on-click #(rf/dispatch [::finish-chooser])}
          "Done"]]])]))
