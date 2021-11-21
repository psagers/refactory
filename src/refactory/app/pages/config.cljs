(ns refactory.app.pages.config
  (:require [datascript.core :as ds]
            [reagent.core :as r]
            [re-frame.core :as rf]
            [re-posh.core :as rp]
            [refactory.app.db :as db]
            [refactory.app.game :as game]
            [refactory.app.pages :as pages]
            [refactory.app.ui :as ui]
            [refactory.app.ui.recipes :as recipes]
            [refactory.app.util :refer [conj-set forall]]))


(defmethod db/ds-migration ::_
  [_ ds]
  (when-not (ds/entid ds [:page/id :config])
    [[:db/add -1 :page/id :config]]))


(defmethod pages/page-config :config
  []
  {:enter [::init-ui]
   :leave [::reset-ui]})


;;
;; Static game data
;;

(def research-category-id->title
  {"ACarapace" "Alien Carapace"
   "AOrgans" "Alien Organs"
   "FlowerPetals" "Flower Petals"
   "PowerSlugs" "Power Slugs"})


(defn- research-id->category
  [id]
  (if-some [[_ category-id] (re-find #"^Research_([A-Za-z0-9]+)" id)]
    (research-category-id->title category-id category-id)
    "Other"))


(defmulti schematic->menu-path
  (comp keyword :kind))


(defmethod schematic->menu-path :milestone
   [{:keys [tier]}]
   [:milestones tier])


(defmethod schematic->menu-path :research
   [{:keys [id]}]
   [:research (research-id->category id)])


(defmethod schematic->menu-path :alternate
   [{:keys [tier]}]
   [:alternates tier])


(defn- -menu-data
  []
  (reduce
    (fn [m {:keys [id] :as schematic}]
      (update-in m (schematic->menu-path schematic) conj-set id))
    {}
    (game/schematics)))


(def menu-data (delay (-menu-data)))


(defmulti menu-path->title first)

(defmethod menu-path->title :milestones [[_ tier]] (str "Tier " tier))
(defmethod menu-path->title :research [[_ category]] category)
(defmethod menu-path->title :alternates [[_ tier]] (if (zero? tier)
                                                     "General"
                                                     (str "Tier " tier)))


(defn- unlock-ratio
  [path checked-ids]
  (let [schematic-ids (get-in @menu-data path)]
    (str (count (filter checked-ids schematic-ids))
         "/"
         (count schematic-ids))))


;;
;; Events
;;

(rf/reg-event-db
  ::init-ui
  (fn [db _]
    (assoc-in db [::ui :menu-path] [:milestones 1])))


(rf/reg-event-db
  ::reset-ui
  (fn [db _]
    (dissoc db ::ui)))


(rf/reg-event-db
  ::set-menu-path
  (fn [db [_ path]]
    (assoc-in db [::ui :menu-path] path)))


(rp/reg-event-ds
  ::set-schematic-unlocked
  (fn [_ [_ schematic-id allowed?]]
    (if allowed?
      [[:db/retract [:page/id :config] :config/locked-schematic-ids schematic-id]]
      [[:db/add [:page/id :config] :config/locked-schematic-ids schematic-id]])))


(rp/reg-event-ds
  ::set-schematic-group-unlocked
  (fn [_ [_ menu-path allowed?]]
    (for [schematic-id (get-in @menu-data menu-path)]
      [(if allowed? :db/retract :db/add) [:page/id :config] :config/locked-schematic-ids schematic-id])))


;;
;; Subscriptions
;;

(rf/reg-sub
  ::ui
  (fn [db _]
    (get db ::ui)))


(rf/reg-sub
  ::menu-path
  :<- [::ui]
  (fn [ui _]
    (get ui :menu-path)))


(rf/reg-sub
  ::visible-schematic-ids
  :<- [::menu-path]
  (fn [path _]
    (sort (get-in @menu-data path))))


(rp/reg-query-sub
  ::locked-schematic-ids
  '[:find [?recipe-id ...]
    :where [?e :page/id :config]
           [?e :config/locked-schematic-ids ?recipe-id]])


(rf/reg-sub
  ::checked-schematic-ids
  :<- [::locked-schematic-ids]
  (fn [locked-ids _]
    (set (remove (set locked-ids) (game/schematic-ids)))))


;;
;; Components
;;


(defn- menu-link
  [path]
  (r/with-let [active-path-sub (rf/subscribe [::menu-path])
               checked-ids-sub (rf/subscribe [::checked-schematic-ids])]
    (let [active-path @active-path-sub
          checked-ids @checked-ids-sub]
      [:a {:class [(when (= path active-path) "is-active")]
           :on-click (ui/link-dispatch [::set-menu-path path])}
       (menu-path->title path)
       " "
       [:span.has-text-grey
        "(" (unlock-ratio path checked-ids) ")"]])))


(defn- sub-menu
  [section]
  [:ul.menu-list
   (forall [sub-key (sort (keys (@menu-data section)))]
     ^{:key sub-key}
     [:li [menu-link [section sub-key]]])])


(defn- main-menu
  []
  [:aside.menu
   [:p.menu-label "Milestones"]
   (sub-menu :milestones)
   [:p.menu-label "Research"]
   (sub-menu :research)
   [:p.menu-label "Alternates"]
   (sub-menu :alternates)])


(defn- schematic-form
  [schematic-id]
  (r/with-let [checked-ids-sub (rf/subscribe [::checked-schematic-ids])]
    (let [schematic (game/id->schematic schematic-id)
          checked-ids @checked-ids-sub]
      [:div.my-6
       [:h2.subtitle
        [:input {:type "checkbox"
                 :checked (contains? checked-ids schematic-id)
                 :on-change #(rf/dispatch [::set-schematic-unlocked schematic-id (-> % .-target .-checked)])}]
        [:span.ml-2 (:display schematic)]]
       [:div.columns.is-multiline
        (forall [recipe-id (:recipe-ids schematic)]
          (let [recipe (game/id->recipe recipe-id)]
            ^{:key recipe-id}
            [:div.column.is-12.is-6-desktop.is-4-widescreen
             [:div.is-flex.is-flex-direction-column
              [:p>b (:display recipe)]
              [recipes/recipe-io recipe-id]]]))]])))


(defn- schematic-forms
  []
  (let [menu-path @(rf/subscribe [::menu-path])
        schematic-ids @(rf/subscribe [::visible-schematic-ids])]
    [:div
     [:div.is-flex
      [:h1.title.mb-0 (menu-path->title menu-path)]
      [:div.dropdown.is-hoverable
       [:div.dropdown-trigger
        [:button.button.is-white
         [:span.icon [:i.bi-gear]]]]
       [:div.dropdown-menu
        [:div.dropdown-content
         [:a.dropdown-item {:on-click (ui/link-dispatch [::set-schematic-group-unlocked menu-path false])}
          "Lock all"]
         [:a.dropdown-item {:on-click (ui/link-dispatch [::set-schematic-group-unlocked menu-path true])}
          "Unlock all"]]]]]

     (forall [schematic-id schematic-ids]
       ^{:key schematic-id}
       [schematic-form schematic-id])]))


(defn root
  []
  [:div.columns
   [:div.column.is-narrow.mr-6
    [main-menu]]
   [:div.column
    [schematic-forms]]])
