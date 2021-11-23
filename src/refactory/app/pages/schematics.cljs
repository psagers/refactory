(ns refactory.app.pages.schematics
  (:require [clojure.string :as str]
            [datascript.core :as ds]
            [re-frame.core :as rf]
            [re-posh.core :as rp]
            [refactory.app.db :as db]
            [refactory.app.game :as game]
            [refactory.app.pages :as pages]
            [refactory.app.ui :as ui]
            [refactory.app.ui.forms :as forms]
            [refactory.app.ui.recipes :as recipes]
            [refactory.app.util :refer [conj-set forall]]))


(defmethod db/ds-migration ::_
  [_ ds]
  (when-not (ds/entid ds [:page/id :config])
    [[:db/add -1 :page/id :config]]))


(defmethod pages/page-config :schematics
  [_]
  {:enter [::init-ui]})


;;
;; Static game data
;;

(def research-category-id->title
  {"ACarapace" "Alien Organisms"
   "AOrgans" "Alien Organisms"
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
   []
   [:alternates :_])


(defn- -menu-data
  []
  (reduce
    (fn [m {:keys [id] :as schematic}]
      (update-in m (schematic->menu-path schematic) conj-set id))
    {}
    (game/schematics)))


(def menu-data (delay (-menu-data)))


(defmulti menu-path->title first)
(defmethod menu-path->title :default [_ _] "")

(defmethod menu-path->title :milestones [[_ tier]] (str "Tier " tier))
(defmethod menu-path->title :research [[_ category]] category)
(defmethod menu-path->title :alternates [_] "Alternates")


(defmulti schematic->title (comp keyword :kind))

(defmethod schematic->title :default
  [{:keys [display]}]
  display)

(defmethod schematic->title :alternate
  [{:keys [display]}]
  (if-some [[_ title] (re-matches #"Alternate:(?:\s+)(.*)" display)]
    title
    display))


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
  [(rf/path ::ui)]
  (fn [ui _]
    (-> ui
        (update :menu-path (fnil identity [:milestones 1]))
        (assoc :search-term ""))))


(rf/reg-event-db
  ::set-menu-path
  [(rf/path ::ui)]
  (fn [ui [_ path]]
    (assoc ui :menu-path path)))


(rf/reg-event-db
  ::set-search-term
  [(rf/path ::ui)]
  (fn [ui [_ term]]
    (assoc ui :search-term (str/lower-case (or term "")))))


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
  ::search-term
  :<- [::ui]
  (fn [ui _]
    (get ui :search-term)))


(defn- schematic-id-matches?
  [term schematic-id]
  (some #(str/includes? % term)
        (-> schematic-id game/id->schematic :search-terms)))


(rf/reg-sub
  ::visible-schematic-ids
  :<- [::menu-path]
  :<- [::search-term]
  (fn [[path term] _]
    (->> (get-in @menu-data path)
         (filter (partial schematic-id-matches? term))
         (sort))))


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
  (let [active-path @(rf/subscribe [::menu-path])
        checked-ids @(rf/subscribe [::checked-schematic-ids])]
    [:a {:class [(when (= path active-path) "is-active")]
         :on-click (ui/link-dispatch [::set-menu-path path])}
     (menu-path->title path)
     " "
     [:span.unlock-ratio
      "(" (unlock-ratio path checked-ids) ")"]]))


(defn- sub-menu
  [section]
  [:ul.menu-list
   (forall [sub-key (sort (keys (@menu-data section)))]
     ^{:key sub-key}
     [:li [menu-link [section sub-key]]])])


(defn- main-menu
  []
  [:aside.menu.schematics-menu
   [:p.menu-label "Milestones"]
   (sub-menu :milestones)
   [:p.menu-label "Research"]
   (sub-menu :research)
   [:p.menu-label "Alternates"]
   (sub-menu :alternates)])


(defn- schematic-form
  [schematic-id]
  (let [schematic (game/id->schematic schematic-id)
        checked-ids @(rf/subscribe [::checked-schematic-ids])]
    [:div.my-4
     [:label.label.is-size-5.has-text-weight-normal
      [:input {:type "checkbox"
               :checked (contains? checked-ids schematic-id)
               :on-change #(rf/dispatch [::set-schematic-unlocked schematic-id (-> % .-target .-checked)])}]
      [:span.ml-2 (schematic->title schematic)]]
     [:div
      (forall [recipe-id (:recipe-ids schematic)]
        (let [recipe (game/id->recipe recipe-id)]
          ^{:key recipe-id}
          [:div.is-flex.is-flex-direction-column.mt-3
           [:p>b (:display recipe)]
           [recipes/recipe-io recipe-id {:info? true}]]))]]))


(defn- schematic-title
  []
  (let [menu-path @(rf/subscribe [::menu-path])]
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
         "Unlock all"]]]]]))


(defn- schematic-forms
  []
  (let [menu-path @(rf/subscribe [::menu-path])
        schematic-ids @(rf/subscribe [::visible-schematic-ids])]
    (when menu-path
      [:div
       [:div.level
        [:div.level-left
         [schematic-title]]
        [:div.level-right
         [forms/search-field {:placeholder "Search schematics"
                              :on-update [::set-search-term]}]]]
       [:div.columns.is-multiline
        (forall [schematic-id schematic-ids]
          ^{:key schematic-id}
          [:div.column.is-12.is-6-desktop.is-4-widescreen
           [schematic-form schematic-id]])]])))


(defn root
  []
  [:div.columns
   [:div.column.is-narrow.mr-6
    [main-menu]]
   [:div.column
    [schematic-forms]]])
