(ns refactory.app.pages.factories
  (:require [clojure.set :as set]
            [clojure.walk :as walk]
            [datascript.core :as ds]
            [fork.re-frame :as fork]
            [re-frame.core :as rf]
            [re-posh.core :as rp]
            [goog.functions :refer [debounce]]
            [loom.alg :as alg]
            [loom.graph :as graph]
            [reagent.core :as r]
            [reagent.ratom :as ratom :refer [reaction]]
            [refactory.app.db :as db]
            [refactory.app.game :as game]
            [refactory.app.pages :as pages]
            [refactory.app.ui :as ui]
            [refactory.app.ui.forms :as forms]
            [refactory.app.ui.items :as items]
            [refactory.app.ui.modal :as modal]
            [refactory.app.ui.recipes :as recipes]
            [refactory.app.util :refer [<sub compare-by forall per-minute]]
            [taoensso.encore :refer [map-vals]]))


(defmethod db/ds-migration ::_
  [_ ds]
  (when-not (ds/entid ds [:page/id :factories])
    [[:db/add -1 :page/id :factories]]))


(defmethod pages/page-config :factories
  []
  {:enter [::enter]
   :leave [::leave]})


(def default-factory-mode (db/attr->default :factory/mode))
(def default-job-count (db/attr->default :job/count))
(def default-overdrive (db/attr->default :instance/overdrive))


(def signed-formatter (js/Intl.NumberFormat. js/undefined
                                             #js {:signDisplay "exceptZero"
                                                  :maximumFractionDigits 2}))

(def total-formatter (js/Intl.NumberFormat. js/undefined
                                            #js {:maximumFractionDigits 2}))


(defn format-signed [v] (. signed-formatter format v))
(defn format-total [v] (. total-formatter format v))
;; (defn format-total [amount] (items/amount->badge amount))


;;
;; Events
;;

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
  (fn [{:keys [ds]} [_ {:factory/keys [title mode]}]]
    (let [title (new-factory-title ds (or title "New Factory"))]
      {:fx [[:transact [{:db/id -1, :factory/title title, :factory/mode mode}
                        {:page/id :factories, :factories/selected -1}]]
            [:dispatch [::modal/hide ::new-factory]]]})))


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
  ::set-factory-mode
  (fn [{:keys [db]} [_ factory-id mode]]
    {:db (update db ::ui dissoc :expanded-job-row)
     :fx [[:transact [[:db/add factory-id :factory/mode mode]]]]}))


(defn- forced-inputs
  [ds factory-id]
  (set (ds/q '[:find [?input ...]
               :in $ ?factory-id
               :where [?factory-id :factory/inputs ?input]]
             ds factory-id)))


(rf/reg-event-fx
  ::select-forced-inputs
  [(rp/inject-cofx :ds)]
  (fn [{:keys [ds]} [_ factory-id]]
    {:fx [[:dispatch [::items/show-chooser {:title "Select inputs"
                                            :explanation "Manually select items that will always be considered inputs to the factory."
                                            :multiple? true
                                            :initial (forced-inputs ds factory-id)
                                            :on-success [::set-forced-inputs factory-id]}]]]}))


(rp/reg-event-ds
  ::set-forced-inputs
  (fn [ds [_ factory-id item-ids]]
    (let [current (forced-inputs ds factory-id)]
      (concat
        (for [item-id (set/difference current item-ids)]
          [:db/retract factory-id :factory/inputs item-id])
        (for [item-id (set/difference item-ids current)]
          [:db/add factory-id :factory/inputs item-id])))))


(rf/reg-event-fx
  ::begin-rename-factory
  (fn [_ [_ factory-id]]
    {:fx [[:dispatch [::modal/show ::rename-factory {:factory-id factory-id
                                                     ::modal/close? false}]]]}))


(rf/reg-event-fx
  ::cancel-rename-factory
  (fn [_ _]
    {:fx [[:dispatch [::modal/hide ::rename-factory]]]}))


(rf/reg-event-fx
  ::finish-rename-factory
  (fn [_ [_ factory-id {:factory/keys [title]}]]
    {:fx [[:transact [(when title [:db/add factory-id :factory/title title])]]
          [:dispatch [::modal/hide ::rename-factory]]]}))


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
      {:fx [[:transact [(if next-factory-id
                          [:db/add [:page/id :factories] :factories/selected next-factory-id]
                          [:db/retract [:page/id :factories] :factories/selected])
                        [:db/retractEntity factory-id]]]]})))


(rp/reg-event-ds
  ::add-job
  (fn [ds [_ factory-id recipe-id]]
    (let [mode (:factory/mode (ds/entity ds factory-id) default-factory-mode)
          continuous? (= mode :continuous)
          [job-id disabled?] (ds/q '[:find [?job-id ?disabled]
                                     :in $ ?factory-id ?recipe-id
                                     :where [?factory-id :factory/jobs ?job-id]
                                            [?job-id :job/recipe-id ?recipe-id]
                                            [(get-else $ ?job-id :job/disabled? false) ?disabled]]
                                   ds factory-id recipe-id)]
      (cond
        (and job-id disabled?)
        [[:db/retract job-id :job/disabled?]]

        (and job-id continuous?)
        [[:db/add job-id :job/instances -1]
         [:db/add -1 :instance/overdrive default-overdrive]]

        job-id
        []

        :else
        [[:db/add factory-id :factory/jobs -1]
         [:db/add -1 :job/recipe-id recipe-id]
         [:db/add -1 :job/count default-job-count]
         [:db/add -1 :job/instances -2]
         [:db/add -2 :instance/overdrive default-overdrive]]))))


(rf/reg-event-fx
  ::set-job-enabled
  (fn [{:keys [db]} [_ job-id enabled?]]
    (let [expanded-job-id (get-in db [::ui :expanded-job-row])]
      (if enabled?
        {:fx [[:transact [[:db/retract job-id :job/disabled?]]]]}
        {:db (cond-> db
               (= expanded-job-id job-id)
               (update ::ui dissoc :expanded-job-row))
         :fx [[:transact [[:db/add job-id :job/disabled? true]]]]}))))


(rp/reg-event-ds
  ::delete-job
  (fn [_ [_ job-id]]
    [[:db/retractEntity job-id]]))


(rp/reg-event-ds
  ::add-instance
  (fn [_ [_ job-id]]
    [[:db/add job-id :job/instances -1]
     [:db/add -1 :instance/overdrive default-overdrive]]))


(rp/reg-event-ds
  ::remove-instance
  (fn [ds [_ job-id]]
    (let [instance-ids (ds/q '[:find [?instance-id ...]
                               :in $ ?job-id
                               :where [?job-id :job/instances ?instance-id]]
                           ds job-id)
          instance-id (first (sort-by - instance-ids))]
      [(when instance-id
         [:db/retractEntity instance-id])])))


(rf/reg-event-db
  ::toggle-expanded-job
  (fn [db [_ job-id]]
    (if (or (nil? job-id)
            (= (get-in db [::ui :expanded-job-row]) job-id))
      (update db ::ui dissoc :expanded-job-row)
      (assoc-in db [::ui :expanded-job-row] job-id))))


(rp/reg-event-ds
  ::delete-instance
  (fn [_ [_ instance-id]]
    [[:db/retractEntity instance-id]]))


(rp/reg-event-ds
  ::set-overdrive
  (fn [_ [_ instance-id value]]
    [(when-some [overdrive (db/decode-value :instance/overdrive value)]
      [:db/add instance-id :instance/overdrive overdrive])]))


(rp/reg-event-ds
  ::set-job-count
  (fn [_ [_ job-id value]]
    [(when-some [value (db/decode-value :job/count value)]
       [:db/add job-id :job/count value])]))


;;
;; Subscriptions
;;

(rf/reg-sub
  ::ui
  (fn [db _]
    (::ui db)))


;; Factories

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
     :pattern '[:db/id :factory/mode :factory/title]
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
     :pattern '[:db/id :factory/mode :factory/title]
     :ids factory-ids}))


(rf/reg-sub
  ::sorted-factories
  :<- [::factories]
  (fn [factories _]
    (sort-by (juxt :factory/title :db/id) factories)))


(rp/reg-pull-sub
  ::factory
  '[:db/id :factory/mode :factory/title])


(rp/reg-query-sub
  ::factory-title
  '[:find ?title .
    :in $ ?factory-id
    :where [?factory-id :factory/title ?title]])


(rp/reg-query-sub
  ::factory-instances
  '[:find [?instance-id ...]
    :in $ ?factory-id
    :where [?factory-id :factory/jobs ?job-id]
           (not [?job-id :job/disabled? true])
           [?job-id :job/instances ?instance-id]])


(rf/reg-sub
  ::factory-instance-count
  (fn [[_ factory-id] _]
    (rf/subscribe [::factory-instances factory-id]))
  (fn [instances _]
    (count instances)))


(rp/reg-query-sub
  ::forced-inputs
  '[:find [?input ...]
    :in $ ?factory-id
    :where [?factory-id :factory/inputs ?input]])


;; Jobs

(rp/reg-sub
  ::job-ids
  (fn [_ [_ factory-id disabled?]]
    {:type :query
     :query [:find '[?job-id ...]
             :in '$ '?factory-id
             :where '[?factory-id :factory/jobs ?job-id]
                    (if disabled?
                      '[?job-id :job/disabled? true]
                      '(not [?job-id :job/disabled? true]))]
     :variables [factory-id]}))


;; Pulls either enabled or disabled jobs with enough information to sort them
;; (:db/id and :job/recipe-id).
(rp/reg-sub
  ::sortable-jobs
  (fn [[_ factory-id disabled?]]
    (rf/subscribe [::job-ids factory-id (boolean disabled?)]))
  (fn [job-ids _]
    {:type :pull-many
     :pattern '[:db/id :job/recipe-id]
     :ids job-ids}))


(rf/reg-sub
  ::sorted-disabled-job-ids
  (fn [[_ factory-id]]
    (rf/subscribe [::sortable-jobs factory-id true]))
  (fn [jobs _]
    (->> (sort-by (comp game/recipe-sort-key :job/recipe-id) jobs)
         (mapv :db/id))))


(rf/reg-sub
  ::input-item-ids
  (fn [[_ factory-id] _]
    (rf/subscribe [::factory-totals-grouped factory-id]))
  (fn [{:keys [input-totals]}]
    (set (keys input-totals))))


(defn- job-edges
  [{job-id :db/id :as job} input-ids]
  (let [recipe (-> job :job/recipe-id game/id->recipe)]
    (concat (for [input (:input recipe)]
              [(:item-id input) job-id])
            (for [output (:output recipe)
                  :when (not (contains? input-ids (:item-id output)))]
              [job-id (:item-id output)]))))


(defn- job-graph
  [jobs input-ids]
  (apply graph/digraph
         (concat (map #(vector :input %) input-ids)
                 (mapcat #(job-edges % input-ids) jobs))))


;; Attempts to sort a factory's jobs topographically. This will be nil if the
;; production graph has any cycles.
(rf/reg-sub
  ::graph-sorted-job-ids
  (fn [[_ factory-id disabled?]]
    {:jobs (rf/subscribe [::sortable-jobs factory-id (boolean disabled?)])
     :input-ids (rf/subscribe [::input-item-ids factory-id])})
  (fn [{:keys [jobs input-ids]} _]
    (some->> (job-graph jobs input-ids)
             (alg/topsort)
             (filter int?))))


;; Sorts a factory's jobs by recipe value.
(rf/reg-sub
  ::value-sorted-job-ids
  (fn [[_ factory-id disabled?]]
    (rf/subscribe [::sortable-jobs factory-id disabled?]))
  (fn [jobs _]
    (->> (sort-by (comp game/recipe-sort-key :job/recipe-id) jobs)
         (mapv :db/id))))


;; Attempts to sort a factory's jobs topographically, but falls back on simple
;; value sorting if necessary.
(rf/reg-sub-raw
  ::smart-sorted-job-ids
  (fn [_ [_ factory-id disabled?]]
    (reaction
      (or (<sub [::graph-sorted-job-ids factory-id disabled?])
          (<sub [::value-sorted-job-ids factory-id disabled?])))))


(rf/reg-sub
  ::sorted-job-ids
  (fn [[_ factory-id] _]
    [(rf/subscribe [::smart-sorted-job-ids factory-id false])
     (rf/subscribe [::value-sorted-job-ids factory-id true])])
  (fn [[enabled disabled] _]
    (concat enabled disabled)))


;; The :mode of the job's factory.
(rp/reg-query-sub
  ::job-mode
  '[:find ?mode .
    :in $ ?job-id
    :where [?factory-id :factory/jobs ?job-id]
           [(get-else $ ?factory-id :factory/mode :continuous) ?mode]])


(rp/reg-query-sub
  ::job-overdrive-modified?
  '[:find ?instance-id .
    :in $ ?job-id
    :where [?job-id :job/instances ?instance-id]
           [?instance-id :instance/overdrive ?overdrive]
           [(not= ?overdrive 100)]])


(rf/reg-sub
  ::expanded-job-id
  :<- [::ui]
  (fn [ui _]
    (:expanded-job-row ui)))


(rp/reg-pull-sub
  ::job
  '[:db/id :job/recipe-id :job/disabled? :job/count])


;; Aggregations

(defn- sorted-item-map
  "Returns an empty map that sorts item-id keys by item value."
  []
  (sorted-map-by (compare-by (comp (juxt :value :display :id) game/id->item))))


(defn- recipe-flows
  "Returns the rate at which each item is consumed and produced by a job.

  This is a list of [item-id rate] 2-tuples (rate = units per minute). Consumed
  resources have negative amounts. Note that some recipes produce and consume
  the same resource, so this isn't quite a map. The factor should account for
  all instances and their overdrive settings."
  [recipe-id factor]
  (let [{:keys [input output duration]} (game/id->recipe recipe-id)]
    (concat (for [{:keys [item-id amount]} input]
              [item-id (per-minute (* (- amount) factor) duration)])
            (for [{:keys [item-id amount]} output]
              [item-id (per-minute (* amount factor) duration)]))))


(defn- recipe-counts
  "Generates the same structure as recipe-flows, but with absolute units.

  In this case, the factor is just the number of times the recipe is produced."
  [recipe-id factor]
  (let [{:keys [input output]} (game/id->recipe recipe-id)]
    (concat (for [{:keys [item-id amount]} input]
              [item-id (* (- amount) factor)])
            (for [{:keys [item-id amount]} output]
              [item-id (* amount factor)]))))


(rp/reg-query-sub
  ::instance-overdrives
  '[:find ?instance-id ?overdrive
    :in $ ?job-id
    :where [?job-id :job/instances ?instance-id]
           [(get-else $ ?instance-id :instance/overdrive 100) ?overdrive]])


;; A job's total production rate as a percentage (100 = 1x).
(rf/reg-sub
  ::job-production-pct
  (fn [[_ job-id] _]
    (rf/subscribe [::instance-overdrives job-id]))
  (fn [overdrives]
    (transduce (map second) + overdrives)))


(rf/reg-sub
  ::job-production-factor
  (fn [[_ job-id] _]
    [(rf/subscribe [::job job-id])
     (rf/subscribe [::job-production-pct job-id])])
  (fn [[job pct] _]
    (if (:job/disabled? job) 0 (/ pct 100))))


;; For cotinuous mode.
(rf/reg-sub
  ::job-flows
  (fn [[_ job-id] _]
    [(rf/subscribe [::job job-id])
     (rf/subscribe [::job-production-factor job-id])])
  (fn [[job factor] _]
    (recipe-flows (:job/recipe-id job) factor)))


;; For fixed mode.
(rf/reg-sub
  ::job-counts
  (fn [[_ job-id] _]
    (rf/subscribe [::job job-id]))
  (fn [job _]
    (let [job-count (if (:job/disabled? job)
                      0
                      (:job/count job default-job-count))]
      (recipe-counts (:job/recipe-id job) job-count))))


(defn- add-to-totals
  ([m] m)
  ([m [item-id quantity]]
   (cond
     (pos? quantity) (update-in m [item-id :out] (fnil + 0) quantity)
     (neg? quantity) (update-in m [item-id :in] (fnil + 0) (Math/abs quantity))
     :else m)))


;; A map from item-id to maps with :in and :out keys, representing the total
;; units of that item consumed and produced in the factory, respectively.
(rf/reg-sub-raw
  ::factory-totals
  (fn [_ [_ factory-id]]
    (reaction
      (let [factory (<sub [::factory factory-id])
            job-ids (<sub [::job-ids factory-id false])
            sub-id (case (:factory/mode factory default-factory-mode)
                     :continuous ::job-flows
                     :fixed ::job-counts
                     ::job-flows)]  ;; not reached
        (transduce (mapcat #(<sub [sub-id %]))
                   add-to-totals
                   {}
                   job-ids)))))


;; A map with keys :input-totals, :local-totals, and :output-totals, each with
;; a sorted sub-map from ::factory-totals.
(rf/reg-sub
  ::factory-totals-grouped
  (fn [[_ factory-id] _]
    [(rf/subscribe [::factory-totals factory-id])
     (rf/subscribe [::forced-inputs factory-id])])
  (fn [[totals input-ids] _]
    (let [input-ids (set input-ids)
          f (fn [[item-id {:keys [in out]}]]
              (cond
                (contains? input-ids item-id) :input-totals
                (nil? out) :input-totals
                (nil? in) :output-totals
                :else :local-totals))]
      (->> (group-by f totals)
           (map-vals #(into (sorted-item-map) %))))))


;; Instances

(rp/reg-query-sub
  ::job-instance-count
  '[:find (count ?instance-id) .
    :in $ ?job-id
    :where [?job-id :job/instances ?instance-id]])


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
  ::job-count-inputs
  :<- [::ui]
  (fn [ui _]
    (get ui :job-count-inputs)))


(rf/reg-sub
  ::job-count-input
  :<- [::job-count-inputs]
  (fn [inputs [_ job-id]]
    (get inputs job-id)))


;;
;; Components
;;

(defn- job-actions
  [job-id]
  (let [{:job/keys [disabled? recipe-id]} @(rf/subscribe [::job job-id])]
    [:div.dropdown.is-hoverable
     [:div.dropdown-trigger
      [:button.button.is-white
       [:span.icon [:i.bi-three-dots]]]]
     [:div.dropdown-menu.has-text-left
      [:div.dropdown-content
       [:a.dropdown-item {:on-click (ui/link-dispatch [::modal/show ::recipes/details {:recipe-id recipe-id}])}
        "Show recipe"]
       [:hr.dropdown-divider]
       (if disabled?
         [:a.dropdown-item {:on-click (ui/link-dispatch [::set-job-enabled job-id true])}
          "Enable"]
         [:a.dropdown-item {:on-click (ui/link-dispatch [::set-job-enabled job-id false])}
          "Disable"])
       [:a.dropdown-item.has-text-danger {:on-click (ui/link-dispatch [::delete-job job-id])}
        "Delete"]]]]))


(defn- overdrive-input
  [instance-id initial]
  (r/with-let [rvalue (ratom/atom (str initial))
               set-overdrive (debounce #(rf/dispatch [::set-overdrive instance-id %]) 250)]
    [:div.field.has-addons.ml-3
     [:div.control
      [:input.input.is-small {:type "number", :size 6
                              :min 0, :max 250
                              :value @rvalue
                              :on-change #(let [value (-> % .-target .-value)]
                                            (reset! rvalue value)
                                            (set-overdrive value))}]]
     [:div.control
      [:a.button.is-static.is-small "%"]]]))


(defn- expanded-job-row
  [job-id]
  [:tr
   [:td {:colSpan 5}
    [:div.is-flex.is-flex-wrap-wrap
     (forall [[instance-id overdrive] (sort @(rf/subscribe [::instance-overdrives job-id]))]
       ^{:key instance-id}
       [overdrive-input instance-id overdrive])]]])


(defn- job-count-cells
  [job-id initial]
  (r/with-let [rvalue (ratom/atom (str initial))
               set-job-count (debounce #(rf/dispatch [::set-job-count job-id %]) 250)]
    (let [job @(rf/subscribe [::job job-id])
          disabled? (:job/disabled? job)]
      [:td {:colSpan 2}
       [:div.field
        [:div.control
         [:input.input.is-small
          {:type "number", :size 5
           :disabled disabled?
           :min 0
           :value @rvalue
           :on-change #(let [value (-> % .-target .-value)]
                         (reset! rvalue value)
                         (set-job-count value))}]]]])))


(defn- job-instance-cells
  [job-id]
  (let [job @(rf/subscribe [::job job-id])
        instance-count @(rf/subscribe [::job-instance-count job-id])
        overdrive-modified? @(rf/subscribe [::job-overdrive-modified? job-id])
        expanded-id @(rf/subscribe [::expanded-job-id])
        disabled? (:job/disabled? job)]
    [:<>
     [:td (or instance-count 0) (when overdrive-modified? [:sup "*"])]
     [:td [:div.buttons.has-addons.is-flex-wrap-nowrap
           [:button.button.is-small {:disabled disabled?
                                     :on-click #(rf/dispatch [::remove-instance job-id])}
            [:span.icon [:i.bi-dash]]]
           [:button.button.is-small {:disabled disabled?
                                     :on-click #(rf/dispatch [::toggle-expanded-job job-id])}
            (if (= expanded-id job-id)
             [:span.icon [:i.bi-chevron-bar-contract]]
             [:span.icon [:i.bi-chevron-bar-expand]])]
           [:button.button.is-small {:disabled disabled?
                                     :on-click #(rf/dispatch [::add-instance job-id])}
            [:span.icon [:i.bi-plus]]]]]]))


(defn- job-row
  [job-id]
  (let [job (<sub [::job job-id])
        production-pct (<sub [::job-production-pct job-id])
        mode (<sub [::job-mode job-id])
        expanded-id (<sub [::expanded-job-id])
        continuous? (= mode :continuous)
        disabled? (:job/disabled? job)
        recipe-id (:job/recipe-id job)
        recipe (game/id->recipe recipe-id)
        builder (-> recipe-id game/id->recipe :builder-id game/id->builder)]
    [:<>
     [:tr.job-row {:class [(when disabled? "has-text-grey")]}
      [:td [:b (:display recipe) (when ^boolean goog.DEBUG (str " [" job-id "]"))] [:br]
           (if disabled? "(disabled)" (:display builder))]

      (if continuous?
        [job-instance-cells job-id]
        [job-count-cells job-id (:job/count job)])

      [:td (recipes/recipe-io recipe-id (cond
                                          disabled? {:multiple 0}
                                          continuous? {:multiple (/ production-pct 100)
                                                       :per-minute? true}
                                          :else {:multiple (:job/count job)}))]
      [:td.has-text-right [job-actions job-id]]]

     (when (= expanded-id job-id)
       [expanded-job-row job-id])]))


(defn- factory-table-view
  [factory-id]
  (let [factory (<sub [::factory factory-id])
        instance-count (<sub [::factory-instance-count factory-id])
        job-ids (<sub [::sorted-job-ids factory-id])
        continuous? (= (:factory/mode factory) :continuous)]
    [:table.table.is-fullwidth.factory-table.is-size-7.is-size-6-widescreen
     [:thead.has-text-left
      [:tr
       [:th "Job"]
       [:th {:colSpan 2}
        (if continuous? "Builders" "Build count")]
       [:th (if continuous? "Flows (/min)" "Unit counts")]
       [:th]]]
     [:tbody
      (forall [job-id job-ids]
        ^{:key job-id}
        [job-row job-id])]
     [:tfoot.has-text-left
      [:tr
       [:th
        [:button.button.is-primary {:on-click #(rf/dispatch [::recipes/show-chooser {:per-minute? continuous?
                                                                                     :on-success [::add-job factory-id]}])}
         [:span.icon [:i.bi-plus-circle]]
         [:span "Add a job"]]]
       [:th (when continuous? (or instance-count 0))]
       [:th {:colSpan 3}]]]]))


(defn- rename-factory-form
  [{:keys [form-id normalize-name values handle-change handle-submit]}]
  [:form {:id form-id
          :on-submit handle-submit}
   ;; This goes first to catch the enter key.
   [:input.button.is-hidden {:type "submit"}]
   [:div.modal-card
    [:header.modal-card-head
     [:p.modal-card-title "Rename factory"]
     [:button.delete {:on-click #(rf/dispatch [::cancel-rename-factory])}]]
    [:section.modal-card-body
     [:div.field
      [:div.control
       [:input.input {:type "text"
                      :name (normalize-name :factory/title)
                      :value (values :factory/title)
                      :min-length 1
                      :max-length 30
                      :autoFocus true
                      :on-change handle-change}]]]]
    [:footer.modal-card-foot.is-justify-content-flex-end
     [:button.button {:on-click #(rf/dispatch [::cancel-rename-factory])}
      "Cancel"]
     [:input.button.is-success {:type "submit", :value "Save"}]]]])


(defmethod modal/content ::rename-factory
  [{:keys [factory-id]}]
  (let [title (<sub [::factory-title factory-id])]
    [fork/form {:keywordize-keys true
                :prevent-default? true
                :clean-on-unmount? true
                :initial-values {:factory/title title}
                :on-submit (forms/on-submit [::finish-rename-factory factory-id])}
     rename-factory-form]))


(defn- factory-actions
  [factory-id]
  (let [{:factory/keys [mode]} @(rf/subscribe [::factory factory-id])]
    [:div.dropdown.is-hoverable
     [:div.dropdown-trigger
      [:button.button.is-white
       [:span.icon [:i.bi-gear]]]]
     [:div.dropdown-menu
      [:div.dropdown-content
       (case mode
         :continuous [:a.dropdown-item {:on-click (ui/link-dispatch [::set-factory-mode factory-id :fixed])}
                      "Fixed mode"]
         :fixed [:a.dropdown-item {:on-click (ui/link-dispatch [::set-factory-mode factory-id :continuous])}
                 "Continuous mode"]
         nil)
       [:a.dropdown-item {:on-click (ui/link-dispatch [::select-forced-inputs factory-id])}
        "Select inputs"]
       [:hr.dropdown-divider]
       [:a.dropdown-item {:on-click (ui/link-dispatch [::begin-rename-factory factory-id])}
        "Rename"]
       [:a.dropdown-item {:on-click (ui/link-dispatch [::duplicate-factory factory-id])}
        "Duplicate"]
       [:hr.dropdown-divider]
       [:a.dropdown-item.has-text-danger {:on-click (ui/link-dispatch [::begin-delete-factory factory-id])}
        "Delete..."]]]]))


(defn- factory-title
  [factory-id]
  (let [factory (<sub [::factory factory-id])]
    (if (some? factory)
      [:<>
       [:h1.title (:factory/title factory)
        (when ^boolean goog.DEBUG (str " [" factory-id "]"))]
       [factory-actions factory-id]]
      [:h1.title "Add your first factory " [:i.bi-arrow-right]])))


(defn- new-factory-form
  [{:keys [form-id normalize-name values errors touched handle-change handle-blur handle-submit]}]
  [:form {:id form-id
          :on-submit handle-submit}
   ;; This goes first to catch the enter key.
   [:input.button.is-hidden {:type "submit"}]
   [:div.modal-card
    [:header.modal-card-head
     [:p.modal-card-title "New factory"]
     [:button.delete {:on-click (ui/link-dispatch [::modal/hide ::new-factory])}]]

    [:section.modal-card-body
     [:div.field
      [:label.label "Name"]
      [:div.control
       [:input.input {:type "text"
                      :name (normalize-name :factory/title)
                      :placeholder "New Factory"
                      :value (values :factory/title)
                      :min-length 1
                      :max-length 30
                      :autoFocus true
                      :on-blur handle-blur
                      :on-change handle-change}]
       (when (touched :factory/title)
         (when-some [error (get errors :factory/title)]
           [:p.help.is-danger "Name " (first error)]))]]

     [:div.field
      [:label.label "Initial mode"]
      [:div.control
       [:label.radio [:input {:type "radio"
                              :name (normalize-name :factory/mode)
                              :value "continuous"
                              :checked (= (values :factory/mode) "continuous")
                              :on-blur handle-blur
                              :on-change handle-change}]
        " Continuous"]
       [:p.help.ml-4 "Model a factory that will operate continuously. All calculations will be in units per minute."]]
      [:div.control
       [:label.radio [:input {:type "radio"
                              :name (normalize-name :factory/mode)
                              :value "fixed"
                              :checked (= (values :factory/mode) "fixed")
                              :on-blur handle-blur
                              :on-change handle-change}]
        " Fixed"]
       [:p.help.ml-4 "Model a production pipeline that will generate a fixed number of outputs. Useful for calculating the inputs needed for one-off production."]]]]

    [:footer.modal-card-foot.is-justify-content-flex-end
     [:button.button {:on-click (ui/link-dispatch [::modal/hide ::new-factory])}
      "Cancel"]
     [:input.button.is-success {:type "submit"
                                :value "Save"}]]]])


(defmethod modal/content ::new-factory
  [_opts]
  [fork/form {:keywordize-keys true
              :prevent-default? true
              :clean-on-unmount? true
              :initial-values {:factory/mode "continuous"}
              :on-submit (forms/on-submit [::new-factory] {:default? true})}
   new-factory-form])


(defn- factory-select
  [selected-factory-id]
  (let [sorted-factories (<sub [::sorted-factories])]
    [:div.dropdown.is-right.is-hoverable
     [:div.dropdown-trigger
      [:button.button {:class [(when (empty? sorted-factories) "is-info")]}
       [:span "Factories"]
       [:span.icon.is-small [:i.bi-chevron-down]]]]
     [:div.dropdown-menu
      [:div.dropdown-content
       (forall [{:keys [db/id factory/title]} sorted-factories]
         ^{:key id}
         [:a.dropdown-item {:on-click (ui/link-dispatch [::select-factory id])
                            :class [(when (= id selected-factory-id) "is-active")]}
          title])
       (when-not (empty? sorted-factories)
         [:hr.dropdown-divider])
       [:a.dropdown-item {:on-click (ui/link-dispatch [::modal/show ::new-factory])}
        "Add a factory"]]]]))


(defn- chooser-link
  [factory-id item-id continuous?]
  [:a.has-text-black {:on-click (ui/link-dispatch [::recipes/show-chooser {:search-text (-> item-id game/id->item :display)
                                                                           :per-minute? continuous?
                                                                           :on-success [::add-job factory-id]}])}
   [:span.icon [:i.bi-plus-circle.is-small]]])


(defn- factory-totals
  [factory-id]
  (let [{:factory/keys [mode]} @(rf/subscribe [::factory factory-id])
        continuous? (= mode :continuous)
        {:keys [input-totals local-totals output-totals]} (<sub [::factory-totals-grouped factory-id])]
    [:div {:style {:position "sticky"
                   :top "1rem"}}
      [:table.table.is-fullwidth.is-size-7.is-size-6-widescreen.has-text-right
       [:thead
        [:tr
         [:th (when continuous? "/min:")]
         [:th "In"]
         [:th "Out"]
         [:th "Net"]
         [:th]]]
       [:tbody.is-family-monospace
        (forall [[item-id {:keys [in out]}] input-totals]
          ^{:key item-id}
          [:tr
           [:td.has-text-left (items/item-icon item-id)]
           [:td (format-total in)]
           [:td (some-> out format-total)]
           [:td (format-total (- in))]
           [:td (chooser-link factory-id item-id continuous?)]])
        (forall [[item-id {:keys [in out]}] local-totals]
          (let [net (- out in)]
            ^{:key item-id}
            [:tr
             [:td.has-text-left (items/item-icon item-id)]
             [:td (format-total in)]
             [:td (format-total out)]
             [:td {:class [(when (neg? net) "has-text-danger")]} (format-total net)]
             [:td (when continuous? (chooser-link factory-id item-id continuous?))]]))
        (forall [[item-id {:keys [out]}] output-totals]
          ^{:key item-id}
          [:tr
           [:td.has-text-left (items/item-icon item-id)]
           [:td]
           [:td (format-total out)]
           [:td (format-total out)]
           [:td (when continuous? (chooser-link factory-id item-id continuous?))]])]]]))


(defn- factory
  [factory-id]
  [:div.columns
   [:div.column.is-three-quarters
    [:div.box
     [factory-table-view factory-id]]
    (when-not (<sub [::graph-sorted-job-ids factory-id])
      [:div.message.is-warning
       [:div.message-body
        "The jobs in this factory can't be ordered by dependencies. This
             is generally caused by cycles in the production line, which you
             should be able to resolve by "
        [:a {:on-click (ui/link-dispatch [::select-forced-inputs factory-id])} "adding"]
        " some designated input items."]])]
   [:div.column.is-one-quarter
    [factory-totals factory-id]]])


(defn- getting-started
  []
  [:section.section
   [:p.block.is-size-5 "Welcome to Refactory."]
   [:p.block.is-size-5
    "This page is for modeling factories with different sets of recipes and
    builders. If you want to jump right in, "
    [:a {:on-click (ui/link-dispatch [::modal/show ::new-factory])}
     "create your first factory"]
    " and start adding jobs. Or head over to "
    [:a.has-text-dark {:on-click (ui/link-dispatch [::pages/switch-to :help])}
     [:span.icon [:i.bi-question-circle]]]
    " for more information."]])


(defn root []
  (let [factory-id (<sub [::selected-factory-id])]
    [:div
     [:div.level
      [:div.level-left
       [:div.level-item [factory-title factory-id]]]
      [:div.level-right
       [:div.level-item [factory-select factory-id]]]]
     (if factory-id
       [factory factory-id]
       [getting-started])]))
