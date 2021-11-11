(ns refactory.app.ui
  "Miscellaneous UI helpers."
  (:require [re-frame.core :as rf]
            [reagent.core :as r]
            [refactory.app.game :as game]
            [refactory.app.util :refer-macros [forall]]))


(defn bi-icon
  "A bootstrap icon."
  [icon-id]
  [:img.bi-icon {:src (str "img/icons/bi/" icon-id ".svg")}])


(defn item->icon-path
  [item]
  (str "img/" (:icon item)))


(rf/reg-event-db
  ::show-modal
  (fn [db [_ modal-id opts]]
    (assoc db ::modal (assoc opts ::modal-id modal-id))))


(rf/reg-event-db
  ::hide-modal
  (fn [db [_ modal-id]]
    (let [{active-id ::modal-id} (::modal db)]
      (if (or (nil? modal-id) (= modal-id active-id))
        (dissoc db ::modal)
        db))))


(rf/reg-sub
  ::modal
  (fn [db _]
    (::modal db)))


(defmulti modal-content ::modal-id)

(defmethod modal-content :default
  [{::keys [modal-id]}]
  [:div.modal-content
   [:div.message.is-danger
    [:div.message-header
     [:p "Internal error"]
     [:button.delete {:on-click #(rf/dispatch [::hide-modal modal-id])}]]
    "Modal ID " (pr-str modal-id) " is not implemented."]])


(defn modal
  [{::keys [modal-id close?] :as opts :or {close? true}}]
  [:div.modal.is-active
   [:div.modal-background]
   [modal-content opts]
   (when close?
    [:button.modal-close.is-large {:on-click #(rf/dispatch [::hide-modal modal-id])}])])


(defn item-io
  "A component indicating an item along with an amount."
  [item-id amount]
  (r/with-let [item (rf/subscribe [::game/item-by-id item-id])]
    [:div.item-io
     [:div.item-io-content
      [:span.icon.is-medium [:img {:src (item->icon-path @item)}]]
      [:span.amount amount]]]))


(defn recipe-io
  "A visual representation of a recipe."
  [recipe-id]
  (r/with-let [recipe (rf/subscribe [::game/recipe-by-id recipe-id])]
    [:div.recipe-io
      (into [:<>]
            (for [{:keys [item-id amount]} (:input @recipe)]
              [item-io item-id amount]))
      (bi-icon "caret-right-fill")
      (into [:<>]
            (for [{:keys [item-id amount]} (:output @recipe)]
              [item-io item-id amount]))]))


(defn recipe-dropdown
  []
  (r/with-let [recipe-ids (rf/subscribe [::game/sorted-recipe-ids])]
    [:div.dropdown.is-active
     [:div.dropdown-trigger
      [:button.button
       [:span "Add a recipe"]
       [:span.icon.is-small.mt-1
        (bi-icon "chevron-down")]]]
     [:div.dropdown-menu {:role "menu"}
      [:div.dropdown-content
       [:div.dropdown-item
        [:div.control
         [:input.input {:type "text"
                        :placeholder "Find a recipe"}]]]
       [:hr.dropdown-divider]
       (for [recipe-id @recipe-ids]
         ^{:key recipe-id}
         [:div.dropdown-item.is-flex.is-align-content-center
          [:a.is-flex-grow-1 [recipe-io recipe-id]]
          [:a.is-flex-grow-0 {:on-click #(rf/dispatch [::show-modal ::recipe {::close? false
                                                                              :recipe-id recipe-id}])}
           [:span.icon.is-large (bi-icon "question-circle")]]])]]]))


(defn- io-details
  [components duration]
  [:div.is-flex.is-flex-wrap-wrap
   (forall [{:keys [item-id amount]} components]
     (let [item @(rf/subscribe [::game/item-by-id item-id])]
       ^{:key item-id}
       [:div.mr-6.mb-4.is-inline-flex.is-align-content-center
        [item-io item-id amount]
        [:span.ml-3
         amount " " (:display item) [:br]
         (game/per-minute amount duration) "/min"]]))])


(defmethod modal-content ::recipe
  [{::keys [modal-id], :keys [recipe-id]}]
  (r/with-let [recipe-sub (rf/subscribe [::game/recipe-by-id recipe-id])]
    (let [{:keys [display input output builder-id duration]} @recipe-sub]
      [:div.modal-card
       [:header.modal-card-head
        [:p.modal-card-title display]
        [:button.delete {:on-click #(rf/dispatch [::hide-modal modal-id])}]]
       [:section.modal-card-body
        [:p.mb-5 (:display @(rf/subscribe [::game/builder-by-id builder-id])) ", " duration "s"]
        [:h2.subtitle.mt-5 "Inputs"]
        (io-details input duration)
        [:h2.subtitle.mt-5 "Outputs"]
        (io-details output duration)]])))
