(ns refactory.app.pages.factories
  (:require [clojure.walk :as walk]
            [datascript.core :as ds]
            [re-frame.core :as rf]
            [re-posh.core :as rp]
            [reagent.core :as r]
            [reagent.ratom :refer [reaction]]
            [refactory.app.db :as db]
            [refactory.app.game :as game]
            [refactory.app.pages :as pages]
            [refactory.app.ui :as ui]
            [refactory.app.ui.modal :as modal]
            [refactory.app.ui.recipes :as recipes]
            [refactory.app.util :refer [compare-by forall per-minute with-places]]))


(when ^boolean goog.DEBUG
  (db/register-app-db-key! ::ui {:optional true}
                           [:map
                            [:factory-name-input {:optional true}
                             :string]

                            [:overdrive-inputs {:optional true}
                              [:map-of :int :int]]]))


(defmethod pages/config :factories
  []
  {:enter [::enter]
   :leave [::leave]})


(rf/reg-event-db
  ::enter
  (fn [db _]
    (dissoc db ::ui)))


(rf/reg-event-db
  ::leave
  (fn [db _]
    (dissoc db ::ui)))


(defn- factory-titles
  [ds]
  (set (ds/q '[:find [?title ...]
               :where [_ :factory/title ?title]]
             ds)))


(defn- new-factory-title
  [ds proposed]
  (let [existing (factory-titles ds)
        [base n] (if-some [[_ head tail] (re-matches #"(.*) (\d+)$" proposed)]
                   [head (js/parseInt tail)]
                   [proposed 0])]
    (loop [n n]
      (let [title (str base (when (pos? n) (str " " n)))]
        (if-not (existing title)
          title
          (recur (inc n)))))))


(rp/reg-event-fx
  ::new-factory
  [(rp/inject-cofx :ds)]
  (fn [{:keys [ds]} _]
    {:fx [[:transact [{:db/id -1, :factory/title (new-factory-title ds "New Factory")}
                      {:page/id :factories, :factories/selected -1}]]]}))


(rp/reg-event-ds
  ::select-factory
  (fn [_ [_ factory-id]]
    [(if (some? factory-id)
       [:db/add [:page/id :factories] :factories/selected factory-id]
       [:db/retract [:page/id :factories] :factories/selected factory-id])]))


(rp/reg-event-ds
  ::select-new-factory
  (fn [_ [_ temp-id {:keys [tempids]}]]
    (when-some [factory-id (get tempids temp-id)]
       [[:db/add [:page/id :factories] :factories/selected factory-id]])))


(rf/reg-event-fx
  ::begin-rename-factory
  [(rp/inject-cofx :ds)]
  (fn [{:keys [db ds]} [_ factory-id]]
    (let [current (:factory/title (ds/entity ds factory-id))]
      {:db (assoc-in db [::ui :factory-name-input] (or current ""))
       :fx [[:dispatch [::modal/show ::rename-factory {:factory-id factory-id
                                                       ::modal/close? false}]]]})))


(rf/reg-event-db
  ::type-factory-name
  (fn [db [_ value]]
    (assoc-in db [::ui :factory-name-input] value)))


(rf/reg-event-fx
  ::cancel-rename-factory
  (fn [{:keys [db]} _]
    {:db (update db ::ui dissoc :factory-name-input)
     :fx [[:dispatch [::modal/hide ::rename-factory]]]}))


(rf/reg-event-fx
  ::finish-rename-factory
  (fn [{:keys [db]} [_ factory-id]]
    (let [value (get-in db [::ui :factory-name-input])]
      {:db (update db ::ui dissoc :factory-name-input)
       :fx [[:transact [[:db/add factory-id :factory/title value]]]
            [:dispatch [::modal/hide ::rename-factory]]]})))


;; To duplicate a factory:
;;
;;   1. Pull the full factory entity, which will include components.
;;   2. Recursively walk the structure and turn all IDs into temp IDs. Also
;;      give it a distinct name.
;;   3. Save it. DataScript will create all new entities from the temp IDs.
(rf/reg-event-fx
  ::duplicate-factory
  [(rf/inject-cofx :ds)]
  (fn [{:keys [ds]} [_ factory-id]]
    (let [factory (ds/pull ds '[*] factory-id)
          new-factory (walk/postwalk #(if (and (map? %) (contains? % :db/id))
                                        (update % :db/id -)
                                        %)
                                     factory)
          new-factory (update new-factory :factory/title (partial new-factory-title ds))]
      {:fx [[:transact+ {:datoms [new-factory]
                         :on-success [::select-new-factory (:db/id new-factory)]}]]})))


(rf/reg-event-fx
  ::begin-delete-factory
  [(rp/inject-cofx :ds)]
  (fn [{:keys [ds]} [_ factory-id]]
    (let [factory-title (:factory/title (ds/entity ds factory-id))]
      {:fx [[:dispatch [::modal/show ::modal/confirm-action {:text (str "Permanently delete \"" factory-title "\"?")
                                                             :button-label "Delete"
                                                             :danger? true
                                                             :on-confirm [::finish-delete-factory factory-id]}]]]})))


(rf/reg-event-fx
  ::finish-delete-factory
  [(rp/inject-cofx :ds)]
  (fn [{:keys [ds]} [_ factory-id]]
    (let [next-factory-id (-> (ds/q '[:find ?title ?id
                                      :in $ ?deleted-id
                                      :where [?id :factory/title ?title]
                                             [(not= ?id ?deleted-id)]]
                                    ds factory-id)
                              (sort)
                              (first)
                              (second))]
      {:fx [[:transact [[:db/add [:page/id :factories] :factories/selected next-factory-id]
                        [:db/retractEntity factory-id]]]]})))


(rp/reg-event-ds
  ::add-job
  (fn [ds [_ factory-id recipe-id]]
    (if-some [job-id (ds/q '[:find ?job-id .
                             :in $ ?factory-id ?recipe-id
                             :where [?factory-id :factory/jobs ?job-id]
                                    [?job-id :job/recipe-id ?recipe-id]]
                           ds factory-id recipe-id)]
      [{:db/id job-id
        :job/instances {:db/id -1}}]

      [{:db/id factory-id
        :factory/jobs [{:db/id -1
                        :job/recipe-id recipe-id
                        :job/instances [{:db/id -2}]}]}])))


(rp/reg-event-ds
  ::set-job-enabled
  (fn [_ [_ job-id enabled?]]
    [(if enabled?
       [:db/retract job-id :job/disabled?]
       [:db/add job-id :job/disabled? true])]))


(rp/reg-event-ds
  ::delete-job
  (fn [_ [_ job-id]]
    [[:db/retractEntity job-id]]))


(rp/reg-event-ds
  ::add-instance
  (fn [_ [_ job-id]]
    [{:db/id job-id
      :job/instances [{:db/id -1}]}]))


(rp/reg-event-ds
  ::delete-instance
  (fn [_ [_ instance-id]]
    [[:db/retractEntity instance-id]]))


(rf/reg-event-fx
  ::edit-overdrive
  [(rf/inject-cofx :ds)]
  (fn [{:keys [db ds]} [_ instance-id]]
    (let [value (:instance/overdrive (ds/entity ds instance-id) 100)]
      {:db (assoc-in db [::ui :overdrive-inputs instance-id] value)})))


(rf/reg-event-db
  ::type-overdrive-input
  (fn [db [_ instance-id value]]
    (cond-> db
      (<= 0 value 250)
      (assoc-in [::ui :overdrive-inputs instance-id] value))))


(rf/reg-event-fx
  ::save-overdrive
  (fn [{:keys [db]} [_ instance-id]]
    (let [value (get-in db [::ui :overdrive-inputs instance-id])]
        {:db (update-in db [::ui :overdrive-inputs] dissoc instance-id)
         :fx [[:transact [(if (= value 100)
                            [:db/retract instance-id :instance/overdrive]
                            [:db/add instance-id :instance/overdrive value])]]]})))


(rf/reg-sub
  ::ui
  (fn [db _]
    (::ui db)))


(rp/reg-query-sub
  ::selected-factory-id
  '[:find ?factory-id .
    :where [?eid :page/id :factories]
           [?eid :factories/selected ?factory-id]])


(rp/reg-sub
  ::selected-factory
  :<- [::selected-factory-id]
  (fn [factory-id _]
    {:type :pull
     :pattern '[:db/id :factory/title]
     :id factory-id}))


(rp/reg-query-sub
  ::factory-ids
  '[:find [?eid ...]
    :where [?eid :factory/title]])


(rp/reg-sub
  ::factories
  :<- [::factory-ids]
  (fn [factory-ids _]
    {:type :pull-many
     :pattern '[:db/id :factory/title]
     :ids factory-ids}))


(rf/reg-sub
  ::sorted-factories
  :<- [::factories]
  (fn [factories _]
    (sort-by (juxt :factory/title :db/id) factories)))


(rp/reg-pull-sub
  ::factory
  '[:db/id :factory/title])


(rf/reg-sub
  ::factory-name-input
  :<- [::ui]
  (fn [ui _]
    (get ui :factory-name-input)))


(rp/reg-query-sub
  ::job-ids
  '[:find [?job-id ...]
    :in $ ?factory-id
    :where [?factory-id :factory/jobs ?job-id]])


;; Jobs and their recipe-ids for a given factory.
(rp/reg-query-sub
  ::job-set
  '[:find ?job-id ?recipe-id
    :in $ ?factory-id
    :where [?factory-id :factory/jobs ?job-id]
           [?job-id :job/recipe-id ?recipe-id]])


;; Takes a set of [job-id recipe-id] tuples and returns a sequence of job-ids
;; sorted by recipe value.
(rf/reg-sub
  ::sorted-job-ids
  (fn [[_ factory-id] _]
    (rf/subscribe [::job-set factory-id]))
  (fn [job-set _]
    (->> (sort-by (comp :value game/id->recipe second) job-set)
         (mapv first))))


(rp/reg-pull-sub
  ::job
  '[:db/id :job/recipe-id :job/disabled?])


(defn- sorted-item-map
  "Returns an empty map that sorts item-id keys by item value."
  []
  (sorted-map-by (compare-by (comp (juxt :value :display :id) game/id->item))))


(defn- recipe-flows
  "Returns the rate at which each item is consumed and produced by a job.

  This is a list of [item-id rate] 2-tuples (rate = units per minute). Consumed
  resources have negative amounts. Note that some recipes produce and consume
  the same resource, so this isn't quite a map. This aggregates all of a job's
  instances with their overdrive settings."
  [recipe-id factor]
  (let [{:keys [input output duration]} (game/id->recipe recipe-id)]
    (concat (for [{:keys [item-id amount]} input]
              [item-id (per-minute (* (- amount) factor) duration)])
            (for [{:keys [item-id amount]} output]
              [item-id (per-minute (* amount factor) duration)]))))


(rp/reg-query-sub
  ::instance-overdrives
  '[:find ?instance-id ?overdrive
    :in $ ?job-id
    :where [?job-id :job/instances ?instance-id]
           [(get-else $ ?instance-id :instance/overdrive 100) ?overdrive]])


(rf/reg-sub
  ::job-production-factor
  (fn [[_ job-id] _]
    [(rf/subscribe [::job job-id])
     (rf/subscribe [::instance-overdrives job-id])
     (rf/subscribe [::overdrive-inputs])])
  (fn [[job overdrives inputs] _]
    (if-not (:job/disabled? job)
      (transduce (comp (map (fn [[k v]]
                              (get inputs k v)))
                       (map #(/ % 100)))
                 (completing +)
                 0
                 overdrives)
      0)))


(rf/reg-sub
  ::job-flows
  (fn [[_ job-id] _]
    [(rf/subscribe [::job job-id])
     (rf/subscribe [::job-production-factor job-id])])
  (fn [[job factor] _]
    (recipe-flows (:job/recipe-id job) factor)))


(defn- add-flow-to-totals
  [m [item-id rate]]
  (cond
    (pos? rate) (update-in m [item-id :out] (fnil + 0) rate)
    (neg? rate) (update-in m [item-id :in] (fnil + 0) (Math/abs rate))
    :else m))


(rf/reg-sub-raw
  ::factory-totals
  (fn [_ [_ factory-id]]
    (reaction
      (let [job-ids @(rf/subscribe [::job-ids factory-id])
            job-flows (map #(deref (rf/subscribe [::job-flows %])) job-ids)]
        (transduce cat (completing add-flow-to-totals) {} job-flows)))))


(rf/reg-sub
  ::factory-input-totals
  (fn [[_ factory-id] _]
    (rf/subscribe [::factory-totals factory-id]))
  (fn [totals _]
    (into (sorted-item-map) (filter (comp nil? :out val)) totals)))


(rf/reg-sub
  ::factory-local-totals
  (fn [[_ factory-id] _]
    (rf/subscribe [::factory-totals factory-id]))
  (fn [totals _]
    (into (sorted-item-map) (filter (comp (every-pred :in :out) val)) totals)))


(rf/reg-sub
  ::factory-output-totals
  (fn [[_ factory-id] _]
    (rf/subscribe [::factory-totals factory-id]))
  (fn [totals _]
    (into (sorted-item-map) (filter (comp nil? :in val)) totals)))


(rp/reg-query-sub
  ::instance-ids
  '[:find [?instance-id ...]
    :in $ ?job-id
    :where [?job-id :job/instances ?instance-id]])


(rf/reg-sub
  ::sorted-instance-ids
  (fn [[_ job-id] _]
    (rf/subscribe [::instance-ids job-id]))
  (fn [instance-ids _]
    (sort instance-ids)))


(rp/reg-pull-sub
  ::instance
  '[:db/id :instance/overdrive])


(rf/reg-sub
  ::overdrive-inputs
  :<- [::ui]
  (fn [ui _]
    (get ui :overdrive-inputs)))


(rf/reg-sub
  ::overdrive-input
  :<- [::overdrive-inputs]
  (fn [inputs [_ instance-id]]
    (get inputs instance-id)))


;;
;; Components
;;

(defn- overdrive-display
  [instance-id]
  (let [instance @(rf/subscribe [::instance instance-id])]
    [:div.field.has-addons
     [:div.control
      [:input.input.is-small
       {:type "text"
        :readOnly true
        :size 5
        :value (str (:instance/overdrive instance 100) "%")}]]

     [:div.control
      [:button.button.is-small
       {:on-click #(rf/dispatch [::edit-overdrive instance-id])}
       [:span.icon [:i.bi-pencil]]]]]))


(defn- overdrive-input
  [instance-id]
  (let [value @(rf/subscribe [::overdrive-input instance-id])]
    [:div.field.has-addons
     [:div.control
      [:input.input.is-small
       {:type "number", :size 5
        :min 0, :max 250
        :value value
        :on-change #(rf/dispatch-sync [::type-overdrive-input instance-id (-> % .-target .-value js/parseInt)])}]]

     [:div.control
      [:button.button.is-small
       {:on-click #(rf/dispatch [::save-overdrive instance-id])}
       [:span.icon [:i.bi-check]]]]]))


(defn- overdrive-field
  [instance-id]
  (let [value @(rf/subscribe [::overdrive-input instance-id])]
    (if value
      [overdrive-input instance-id]
      [overdrive-display instance-id])))


(defn- instance-rows
  [job-id]
  (let [job @(rf/subscribe [::job job-id])
        instance-ids @(rf/subscribe [::sorted-instance-ids job-id])
        builder (-> job :job/recipe-id game/id->recipe :builder-id game/id->builder)]
    [:<>
     (for [instance-id instance-ids]
       ^{:key instance-id}
       [:div.field.is-horizontal.is-flex
        [:div.field-label.is-small
         (:display builder)]

        [:div.field-body.ml-2
         [overdrive-field instance-id]]

        [:button.button.is-white.is-small
         {:on-click #(rf/dispatch [::delete-instance instance-id])}
         [:span.icon [:i.bi-x-lg]]]])]))


(defn- job-actions
  [job-id]
  (let [{:job/keys [disabled? recipe-id]} @(rf/subscribe [::job job-id])]
    [:div.dropdown.is-hoverable
     [:div.dropdown-trigger
      [:button.button.is-white
       [:span.icon [:i.bi-gear]]]]
     [:div.dropdown-menu
      [:div.dropdown-content
       [:a.dropdown-item {:href "#"
                          :on-click (ui/link-dispatch [::modal/show ::recipes/details {:recipe-id recipe-id}])}
        "Show recipe"]
       [:a.dropdown-item {:href "#"
                          :on-click (ui/link-dispatch [::add-instance job-id])}
        "Add builder"]
       [:hr.dropdown-divider]
       (if disabled?
         [:a.dropdown-item {:href "#"
                            :on-click (ui/link-dispatch [::set-job-enabled job-id true])}
          "Enable"]
         [:a.dropdown-item {:href "#"
                            :on-click (ui/link-dispatch [::set-job-enabled job-id false])}
          "Disable"])
       [:a.dropdown-item.has-text-danger {:href "#"
                                          :on-click (ui/link-dispatch [::delete-job job-id])}
        "Delete"]]]]))


(defn- job-flows
  [job-id]
  (let [flows @(rf/subscribe [::job-flows job-id])]
    [:div
     (forall [[item-id rate] flows]
       ^{:key item-id}
       [:div.is-flex.is-justify-content-space-between
        [:div.is-inline-flex (recipes/item-icon item-id) [:span.ml-2 (:display (game/id->item item-id))]]
        [:span.is-family-monospace (when (pos? rate) "+") rate "/min"]])]))


(defn- job-card
  [job-id]
  (let [job @(rf/subscribe [::job job-id])
        disabled? (:job/disabled? job)
        recipe (game/id->recipe (:job/recipe-id job))]
    [:div.card.job-card
     [:div.card-header
      [:div.card-header-icon (recipes/item-icon (-> recipe :output first :item-id)
                                                {:class ["is-medium"]})]
      [:div.card-header-title (:display recipe) (when disabled? " (DISABLED)")]
      [:div.card-header-icon
       [job-actions job-id]]]
     [:div.card-content
      [instance-rows job-id]
      [:hr.hr]
      [job-flows job-id]]]))


(defn- factory-jobs
  [factory-id]
  (let [job-ids @(rf/subscribe [::sorted-job-ids factory-id])]
      [:div
       [:div.columns.is-multiline
        (forall [job-id job-ids]
          ^{:key job-id}
          [:div.column.is-4-widescreen.is-6-desktop
           [job-card job-id]])

        [:div.column.is-4-widescreen.is-6-desktop
         [:div.card
          [:div.card-header
           [:div.card-header-title]
           [:div.card-header-icon [:span.icon.is-medium]]]
          [:div.card-content.has-text-centered
           [:button.button.is-primary {:on-click #(rf/dispatch [::recipes/show-chooser {:on-success [::add-job factory-id]}])}
            "Add a recipe"]]]]]]))


(defmethod modal/content ::rename-factory
  [{:keys [factory-id]}]
  (r/with-let [input-value-sub (rf/subscribe [::factory-name-input])]
    (let [input-value @input-value-sub]
      [:div.modal-card
       [:header.modal-card-head
        [:p.modal-card-title "Rename factory"]
        [:button.delete {:on-click #(rf/dispatch [::cancel-rename-factory])}]]
       [:section.modal-card-body
        [:div.field
         [:div.control
          [:input.input {:type "text"
                         :value input-value
                         :min-length 1
                         :max-length 30
                         :autoFocus true
                         :on-change #(rf/dispatch-sync [::type-factory-name (-> % .-target .-value)])}]]]]
       [:footer.modal-card-foot.is-justify-content-end
        [:button.button {:on-click #(rf/dispatch [::cancel-rename-factory])}
         "Cancel"]
        [:button.button.is-success {:on-click #(rf/dispatch [::finish-rename-factory factory-id])}
         "Save"]]])))


(defn- factory-actions
  [factory-id]
  [:div.dropdown.is-hoverable
   [:div.dropdown-trigger
    [:button.button.is-white
     [:span.icon [:i.bi-gear]]]]
   [:div.dropdown-menu
    [:div.dropdown-content
     [:a.dropdown-item {:on-click (ui/link-dispatch [::begin-rename-factory factory-id])}
      "Rename"]
     [:a.dropdown-item {:on-click (ui/link-dispatch [::duplicate-factory factory-id])}
      "Duplicate"]
     [:hr.dropdown-divider]
     [:a.dropdown-item.has-text-danger {:on-click (ui/link-dispatch [::begin-delete-factory factory-id])}
      "Delete..."]]]])


(defn- factory-title
  [factory-id]
  (let [factory @(rf/subscribe [::factory factory-id])]
    (if (some? factory)
      [:<>
       [:h1.title (:factory/title factory)]
       [factory-actions factory-id]]
      [:h1.title "Add your first factory " [:i.bi-arrow-right]])))


(defn- factory-select
  [selected-factory-id]
  (r/with-let [sorted-factories-sub (rf/subscribe [::sorted-factories])]
    (let [sorted-factories @sorted-factories-sub]
      [:div.dropdown.is-right.is-hoverable
       [:div.dropdown-trigger
        [:button.button {:class [(when (empty? sorted-factories) "is-info")]}
         [:span "Factories"]
         [:span.icon.is-small [:i.bi-chevron-down]]]]
       [:div.dropdown-menu
        [:div.dropdown-content
         (forall [{:keys [db/id factory/title]} sorted-factories]
           ^{:key id}
           [:a.dropdown-item {:href "#"
                              :on-click (ui/link-dispatch [::select-factory id])
                              :class [(when (= id selected-factory-id) "is-active")]}
            title])
         (when-not (empty? sorted-factories)
           [:hr.dropdown-divider])
         [:a.dropdown-item {:href "#"
                            :on-click (ui/link-dispatch [::new-factory])}
          "Add a factory"]]]])))


(defn- chooser-link
  [factory-id item-id]
  [:a.has-text-black {:href "#"
                      :on-click (ui/link-dispatch [::recipes/show-chooser {:search-term (-> item-id game/id->item :display)
                                                                           :on-success [::add-job factory-id]}])}
   [:span.icon [:i.bi-file-plus.is-small]]])


(defn- factory-totals
  [factory-id]
  (let [input-totals @(rf/subscribe [::factory-input-totals factory-id])
        local-totals @(rf/subscribe [::factory-local-totals factory-id])
        output-totals @(rf/subscribe [::factory-output-totals factory-id])]
    [:div.ml-auto.has-text-right {:style {:position "sticky"
                                          :top "1rem"}}
      [:table.table.ml-auto
       [:thead
        [:tr
         [:th "/min:"]
         [:th "In"]
         [:th "Out"]
         [:th "Net"]
         [:th]]]
       [:tbody.is-family-monospace
        (forall [[item-id {:keys [in]}] input-totals]
          ^{:key item-id}
          [:tr
           [:td (recipes/item-icon item-id)]
           [:td in]
           [:td]
           [:td (- in)]
           [:td (chooser-link factory-id item-id)]])
        (forall [[item-id {:keys [in out]}] local-totals]
          (let [net (with-places (- out in) 2)]
            ^{:key item-id}
            [:tr
             [:td (recipes/item-icon item-id)]
             [:td in]
             [:td out]
             [:td {:class [(when (neg? net) "has-text-danger")]} net]
             [:td (chooser-link factory-id item-id)]]))
        (forall [[item-id {:keys [out]}] output-totals]
          ^{:key item-id}
          [:tr
           [:td (recipes/item-icon item-id)]
           [:td]
           [:td out]
           [:td out]
           [:td (chooser-link factory-id item-id)]])]]]))


(defn root []
  (r/with-let [factory-id-sub (rf/subscribe [::selected-factory-id])]
    (let [factory-id @factory-id-sub]
      [:div
       [:div.level
        [:div.level-left
         [:div.level-item [factory-title factory-id]]]
        [:div.level-right
         [:div.level-item [factory-select factory-id]]]]
       (when factory-id
         [:div.columns
          [:div.column.is-three-quarters
           [factory-jobs factory-id]]
          [:div.column.is-one-quarter
           [factory-totals factory-id]]])])))
