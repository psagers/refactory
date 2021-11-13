(ns refactory.app.pages.factories
  (:require [com.rpl.specter :as t]
            [cljs.reader :as reader]
            [malli.core :as m]
            [re-frame.core :as rf]
            [reagent.core :as r]
            [refactory.app.db :as db]
            [refactory.app.game :as game]
            [refactory.app.pages :as pages]
            [refactory.app.ui :as ui]
            [refactory.app.ui.recipes :as recipes]
            [refactory.app.util :refer-macros [forall]]))


(when ^boolean goog.DEBUG
  (let [Instance
        [:map
         [:overdrive [:double {:min 0, :max 2.5}]]]

        Job
        [:map
         [:recipe-id :string]
         [:disabled? {:optional true} :boolean]
         [:instances [:sequential Instance]]]

        Factory
        [:map
         [:id :uuid]
         [:display :string]
         [:jobs [:map-of :string Job]]]  ;; Keyed by recipe-id

        Data
        [:map
         [:factories [:map-of :uuid Factory]]
         [:active-factory-id :uuid]
         [:overdrive-inputs [:map-of [:tuple :uuid :string :int]
                                     [:int {:min 0, :max 250}]]]]]

    (db/register-db-entry! ::data {:optional true} Data)))


(defmethod pages/config :factories
  [_]
  {:enter [::enter]})
   ;; :leave [::leave]})


(defn- ->instance
  []
  {:overdrive 1})


(defn- ->job
  [recipe-id]
  {:recipe-id recipe-id
   :instances [(->instance)]})


(defn- ->factory
  []
  {:id (random-uuid)
   :display "New Factory"
   :jobs {}})


(defn- initial-factories
  "Generates initial factory data when we're starting from scratch."
  []
  (let [factory (->factory)]
    {:factories {(:id factory) factory}
     :active-factory-id (:id factory)
     :overdrive-inputs {}}))


(defn- load-factories
  "TODO: load factories from local storage."
  []
  (initial-factories))


(rf/reg-event-db
  ::enter
  (fn [db _]
    (cond-> db
      (not (contains? db ::data))
      (assoc ::data (load-factories)))))


(rf/reg-event-db
  ::add-job
  [(rf/path [::data :factories])]
  (fn [factories [_ factory-id recipe-id]]
    (t/multi-transform [(t/must factory-id :jobs)
                        (t/if-path (t/must recipe-id)
                          [(t/must recipe-id :instances) (t/terminal #(conj % (->instance)))]
                          (t/terminal #(assoc % recipe-id (->job recipe-id))))]
                       factories)))


(rf/reg-event-db
  ::delete-job
  [(rf/path [::data :factories])]
  (fn [factories [_ factory-id recipe-id]]
    (t/setval [(t/must factory-id :jobs) (t/map-key recipe-id)]
              t/NONE
              factories)))


(rf/reg-event-db
  ::add-instance
  [(rf/path [::data :factories])]
  (fn [factories [_ factory-id recipe-id]]
    (t/setval [(t/must factory-id :jobs recipe-id :instances) t/END]
              [(->instance)]
              factories)))


(rf/reg-event-db
  ::edit-overdrive
  [(rf/path [::data])]
  (fn [data [_ factory-id recipe-id idx :as query]]
    (let [value (get-in data [:factories factory-id :jobs recipe-id :instances idx :overdrive])
          tuple (vec (rest query))]
      (assoc-in data [:overdrive-inputs tuple] (* value 100)))))


(rf/reg-event-db
  ::set-overdrive
  [(rf/path [::data :overdrive-inputs])]
  (fn [inputs [_ factory-id recipe-id idx value]]
    (if (m/validate [:int {:min 0, :max 250}] value)
      (assoc inputs [factory-id recipe-id idx] value)
      inputs)))


(rf/reg-event-db
  ::save-overdrive
  [(rf/path [::data])]
  (fn [data [_ factory-id recipe-id idx :as query]]
    (let [tuple (vec (rest query))
          value (-> (get-in data [:overdrive-inputs tuple])
                    (/ 100))]
      (t/multi-transform (t/multi-path [(t/keypath :factories factory-id :jobs recipe-id :instances idx :overdrive) (t/terminal-val value)]
                                       [:overdrive-inputs (t/map-key tuple) (t/terminal-val t/NONE)])
                         data))))


(rf/reg-event-db
  ::delete-instance
  [(rf/path [::data])]
  (fn [data [_ factory-id recipe-id idx :as query]]
    (let [tuple (vec (rest query))]
      (t/multi-transform (t/multi-path [(t/must :factories factory-id :jobs recipe-id :instances idx) (t/terminal-val t/NONE)]
                                       [:overdrive-inputs (t/map-key tuple) (t/terminal-val t/NONE)])
                         data))))


(rf/reg-sub
  ::data
  (fn [db _]
    (::data db)))


(rf/reg-sub
  ::active-factory-id
  :<- [::data]
  (fn [data _]
    (:active-factory-id data)))


(rf/reg-sub
  ::factories
  :<- [::data]
  (fn [data _]
    (:factories data)))


(rf/reg-sub
  ::factory
  :<- [::factories]
  (fn [factories [_ factory-id]]
    (get factories factory-id)))


(rf/reg-sub
  ::sorted-recipe-ids
  (fn [[_ factory-id] _]
    (rf/subscribe [::factory factory-id]))
  (fn [factory _]
    (sort-by (comp :value game/id->recipe)
             (keys (:jobs factory)))))


(rf/reg-sub
  ::job
  (fn [[_ factory-id _recipe-id] _]
    (rf/subscribe [::factory factory-id]))
  (fn [factory [_ _factory-id recipe-id]]
    (get-in factory [:jobs recipe-id])))


(rf/reg-sub
  ::job-instance
  (fn [[_ factory-id recipe-id _idx] _]
    (rf/subscribe [::job factory-id recipe-id]))
  (fn [job [_ _factory-id _recipe-id idx]]
    (get-in job [:instances idx])))


(rf/reg-sub
  ::overdrive-inputs
  :<- [::data]
  (fn [data _]
    (:overdrive-inputs data)))


(rf/reg-sub
  ::overdrive-input
  :<- [::overdrive-inputs]
  (fn [inputs [_ & tuple]]
    (get inputs tuple)))


;;
;; Components
;;

(defn- overdrive-display
  [factory-id recipe-id idx]
  (r/with-let [instance-sub (rf/subscribe [::job-instance factory-id recipe-id idx])]
    (let [instance @instance-sub]
      [:div.field.has-addons
       [:div.control
        [:input.input.is-small
         {:type "text"
          :readOnly true
          :size 5
          :value (str (* (:overdrive instance) 100) "%")}]]

       [:div.control
        [:button.button.is-small
         {:on-click #(rf/dispatch [::edit-overdrive factory-id recipe-id idx])}
         [:span.icon (ui/bi-icon "pencil")]]]])))


(defn- overdrive-input
  [factory-id recipe-id idx]
  (r/with-let [input-value-sub (rf/subscribe [::overdrive-input factory-id recipe-id idx])]
    (let [input-value @input-value-sub]
      [:div.field.has-addons
       [:div.control
        [:input.input.is-small
         {:type "number", :size 5
          :min 0, :max 250
          :value input-value
          :on-change #(rf/dispatch [::set-overdrive factory-id recipe-id idx (-> % .-target .-value js/parseInt)])}]]

       [:div.control
        [:button.button.is-small
         {:on-click #(rf/dispatch [::save-overdrive factory-id recipe-id idx])}
         [:span.icon (ui/bi-icon "check")]]]])))


(defn- overdrive-field
  [factory-id recipe-id idx]
  (r/with-let [input-value-sub (rf/subscribe [::overdrive-input factory-id recipe-id idx])]
    (let [input-value @input-value-sub]
      (if input-value
        [overdrive-input factory-id recipe-id idx]
        [overdrive-display factory-id recipe-id idx]))))


(defn- job-instance-rows
  [factory-id recipe-id]
  (r/with-let [job-sub (rf/subscribe [::job factory-id recipe-id])]
    (let [job @job-sub
          builder (-> recipe-id game/id->recipe :builder-id game/id->builder)]
      (into [:<>]
            (for [idx (range (count (:instances job)))]
              [:div.field.is-horizontal
               [:div.field-label.is-small
                (:display builder)]

               [:div.field-body
                [overdrive-field factory-id recipe-id idx]

                [:button.button.is-white.is-small
                 {:on-click #(rf/dispatch [::delete-instance factory-id recipe-id idx])}
                 [:span.icon.is-small (ui/bi-icon "trash")]]]])))))

                ;; [:div.dropdown.is-hoverable
                ;;  [:div.dropdown-trigger
                ;;   [:button.button.is-white.is-size-7
                ;;    [:span.icon.is-small (ui/bi-icon "gear")]]]
                ;;  [:div.dropdown-menu.has-text-left
                ;;   [:div.dropdown-content
                ;;    [:button.button.is-white.dropdown-item {:on-click #(rf/dispatch [::edit-overdrive factory-id recipe-id idx])}
                ;;     "Edit"]
                ;;    [:button.button.is-white.dropdown-item {:on-click #(rf/dispatch [::delete-instance factory-id recipe-id idx])}
                ;;     "Delete"]]]]]])))))


(defn- job-card
  [factory-id recipe-id]
  (r/with-let [];; factory-sub (rf/subscribe [::factory factory-id])
    (let [recipe (game/id->recipe recipe-id)]
      [:div.card.job-card
       [:div.card-header
        [:div.card-header-title (:display recipe)]
        [:div.card-header-icon (recipes/item-icon (-> recipe :output first :item-id)
                                                  {:class ["is-medium"]})]]
       [:div.card-content
        (job-instance-rows factory-id recipe-id)

        [:div.has-text-centered
         [:button.button.is-ghost {:on-click #(rf/dispatch [::add-instance factory-id recipe-id])}
          [:span.icon (ui/bi-icon "plus" 24)]]]
        [:hr.hr]]

       [:div.card-footer
        [:button.button.is-ghost.card-footer-item {:on-click #(rf/dispatch [::delete-job factory-id recipe-id])}
         [:span.icon (ui/bi-icon "trash")]]]])))


(defn- factory-details
  [factory-id]
  (r/with-let [factory-sub (rf/subscribe [::factory factory-id])
               recipe-ids-sub (rf/subscribe [::sorted-recipe-ids factory-id])]
    (let [{:keys [display]} @factory-sub
          recipe-ids @recipe-ids-sub]
      [:div
       [:h1.title display]
       [:div.columns.is-multiline
        (forall [recipe-id recipe-ids]
          ^{:key recipe-id}
          [:div.column.is-3-widescreen.is-4-desktop
           [job-card factory-id recipe-id]])

        [:div.column.is-3-widescreen.is-4-desktop
         [:div.card
          [:div.card-header
           [:div.card-header-title]
           [:div.card-header-icon [:span.icon.is-medium]]]
          [:div.card-content.has-text-centered
           [:button.button.is-primary {:on-click #(rf/dispatch [::recipes/show-chooser {:on-success [::add-job factory-id]}])}
            "Add a recipe"]]]]]])))


(defn factory-list
  []
  [:h1.title.has-text-centered "Factory list"])


(defn root []
  (r/with-let [factory-id-sub (rf/subscribe [::active-factory-id])]
    (let [factory-id @factory-id-sub]
      (if factory-id
        [factory-details factory-id]
        [factory-list]))))
