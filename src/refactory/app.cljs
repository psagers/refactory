(ns refactory.app
  (:require [day8.re-frame.http-fx]
            [re-frame.core :as rf]
            [reagent.core :as r]
            [reagent.dom :as rdom]
            [refactory.app.factories :as factories]
            [refactory.app.game :as game]
            [refactory.app.ui :as ui]))


(defn recipe-row
  [recipe-id]
  (r/with-let [recipe (rf/subscribe [::game/recipe-by-id recipe-id])]
    [:tr
     [:td (:display @recipe)]
     (into [:td]
           (interpose [:br]
                      (for [{:keys [item-id amount]} (:input @recipe)]
                        (let [item @(rf/subscribe [::game/item-by-id item-id])]
                          (str amount " " (:display item)
                               " (" (game/per-minute amount (:duration @recipe)) "/min)")))))
     (into [:td]
           (interpose [:br]
                      (for [{:keys [item-id amount]} (:output @recipe)]
                        (let [item @(rf/subscribe [::game/item-by-id item-id])]
                          (str amount " " (:display item)
                               " (" (game/per-minute amount (:duration @recipe)) "/min)")))))
     (into [:td]
           (interpose [:br]
                      (for [builder-id (:builders @recipe)]
                        (let [builder @(rf/subscribe [::game/builder-by-id builder-id])]
                          (:display builder)))))
     [:td (:value @recipe)]
     [:td [ui/recipe-io (:id @recipe)]]]))


(defn recipe-table
  []
  (r/with-let [recipes (rf/subscribe [::game/recipes])]
    [:section.section
     [:h1.title "Hello, Refactory!"]
     [:table.table
      [:thead
       [:tr
        [:th "Recipe"]
        [:th "Input"]
        [:th "Output"]
        [:th "Produced in"]
        [:th "Value"]
        [:th]]]
      [:tbody
       (doall (for [recipe-id (map :id @recipes)]
                ^{:key recipe-id}
                [recipe-row recipe-id]))]]]))


(rf/reg-event-db
  ::toggle-navbar
  (fn [db [_ navbar-id]]
    (update db ::expanded-navbars #(if (% navbar-id) (disj % navbar-id) (conj % navbar-id)))))


(rf/reg-sub
  ::navbar-expanded?
  (fn [db [_ navbar-id]]
    (contains? (::expanded-navbars db) navbar-id)))


(rf/reg-sub
  ::page
  (fn [db _]
    (::page db)))


(rf/reg-event-db
  ::set-page
  (fn [db [_ page]]
    (assoc db ::page page)))


(defn navbar []
  (r/with-let [page (rf/subscribe [::page])
               expanded? (rf/subscribe [::navbar-expanded? :main])]
    [:nav#main-navbar.navbar {:role "navigation"}
     [:div.navbar-brand
      [:div.navbar-item.has-background-dark.has-text-white-ter "Refactory"]
      [:a.navbar-burger {:role "button"
                         :class [(when @expanded? "is-active")]
                         :on-click #(rf/dispatch [::toggle-navbar :main])}
       [:span] [:span] [:span]]]
     [:div.navbar-menu {:class [(when @expanded? "is-active")]}
      [:div.navbar-start
       [:a.navbar-item.is-tab {:class [(when (= @page :factories) "is-active")]
                               :on-click #(rf/dispatch [::set-page :factories])}
        "Factories"]
       [:a.navbar-item.is-tab {:class [(when (= @page :explore) "is-active")]
                               :on-click #(rf/dispatch [::set-page :explore])}
        "What can I build?"]]

      [:div.navbar-end
       [:a.navbar-item.is-tab {:class [(when (= @page :config) "is-active")]
                               :on-click #(rf/dispatch [::set-page :config])}
        "Config"]
       [:a.navbar-item.is-tab {:class [(when (= @page :help) "is-active")]
                               :on-click #(rf/dispatch [::set-page :help])}
        (ui/bi-icon "question-circle")]]]]))


(defn- nyi []
  [:section.section
   [:h1.title] "NYI"])


(defn root
  []
  (r/with-let [page (rf/subscribe [::page])
               modal (rf/subscribe [::ui/modal])]
    [:<>
     [navbar]
     (case @page
       :factories [factories/root]
       [nyi])

     (when @modal
       [ui/modal @modal])]))


(rf/reg-event-fx
  ::init
  (fn [_ _]
    {:db {::page :factories
          ::expanded-navbars #{}}
     :fx [[:dispatch [::game/fetch-game-data]]]}))


(defn- ^:dev/after-load install
  []
  (rdom/render [root] (js/document.getElementById "app")))


(defn init
  []
  (rf/dispatch-sync [::init])

  (install))
