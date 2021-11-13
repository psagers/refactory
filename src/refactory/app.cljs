(ns refactory.app
  (:require [day8.re-frame.http-fx]
            [re-frame.core :as rf]
            [reagent.core :as r]
            [reagent.dom :as rdom]
            [refactory.app.db :as db]
            [refactory.app.game :as game]
            [refactory.app.pages :as pages]
            [refactory.app.pages.factories :as factories]
            [refactory.app.ui :as ui]
            [refactory.app.ui.modal :as modal]))
            ;; [refactory.app.ui.recipes :as recipes]
            ;; [refactory.app.util :refer [per-minute]]))


(when ^boolean goog.DEBUG
  (db/register-db-entry! ::page :keyword)
  (db/register-db-entry! ::expanded-navbars [:set :keyword]))


;; (defn recipe-row
;;   [recipe-id]
;;   (r/with-let [recipe (rf/subscribe [::game/recipe-by-id recipe-id])]
;;     [:tr
;;      [:td (:display @recipe)]
;;      (into [:td]
;;            (interpose [:br]
;;                       (for [{:keys [item-id amount]} (:input @recipe)]
;;                         (let [item @(rf/subscribe [::game/item-by-id item-id])]
;;                           (str amount " " (:display item)
;;                                " (" (per-minute amount (:duration @recipe)) "/min)")))))
;;      (into [:td]
;;            (interpose [:br]
;;                       (for [{:keys [item-id amount]} (:output @recipe)]
;;                         (let [item @(rf/subscribe [::game/item-by-id item-id])]
;;                           (str amount " " (:display item)
;;                                " (" (per-minute amount (:duration @recipe)) "/min)")))))
;;      (into [:td]
;;            (interpose [:br]
;;                       (for [builder-id (:builders @recipe)]
;;                         (let [builder @(rf/subscribe [::game/builder-by-id builder-id])]
;;                           (:display builder)))))
;;      [:td (:value @recipe)]
;;      [:td [recipes/recipe-io (:id @recipe)]]]))


;; (defn recipe-table
;;   []
;;   (r/with-let [recipes (rf/subscribe [::game/recipes])]
;;     [:section.section
;;      [:h1.title "Hello, Refactory!"]
;;      [:table.table
;;       [:thead
;;        [:tr
;;         [:th "Recipe"]
;;         [:th "Input"]
;;         [:th "Output"]
;;         [:th "Produced in"]
;;         [:th "Value"]
;;         [:th]]]
;;       [:tbody
;;        (doall (for [recipe-id (map :id @recipes)]
;;                 ^{:key recipe-id}
;;                 [recipe-row recipe-id]))]]]))


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


(rf/reg-event-fx
  ::switch-to
  (fn [db [_ page]]
    (let [old (::page db)]
      {:fx [(when-some [leave (:leave (pages/config old))]
              [:dispatch leave])
            (when-some [enter (:enter (pages/config page))]
              [:dispatch enter])
            [:dispatch [::set-page page]]]})))


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
                               :on-click #(rf/dispatch [::switch-to :factories])}
        "Factories"]
       [:a.navbar-item.is-tab {:class [(when (= @page :explore) "is-active")]
                               :on-click #(rf/dispatch [::switch-to :explore])}
        "What can I build?"]]

      [:div.navbar-end
       [:a.navbar-item.is-tab {:class [(when (= @page :config) "is-active")]
                               :on-click #(rf/dispatch [::switch-to :config])}
        "Config"]
       [:a.navbar-item.is-tab {:class [(when (= @page :help) "is-active")]
                               :on-click #(rf/dispatch [::switch-to :help])}
        (ui/bi-icon "question-circle")]]]]))


(defn- nyi []
  [:section.section
   [:h1.title.has-text-centered "NYI"]])


(defn root
  []
  (r/with-let [page (rf/subscribe [::page])
               modal-opts (rf/subscribe [::modal/modal])]
    [:<>
     [navbar]
     [:div.container.is-fluid.mt-5
      (case @page
        :factories [factories/root]
        [nyi])]

     (when @modal-opts
       [modal/modal @modal-opts])]))


(rf/reg-event-fx
  ::install-ui
  (fn [_ _]
    {:fx [[::install-ui]
          [:dispatch [::switch-to :factories]]]}))


(rf/reg-event-fx
  ::init
  (fn [_ _]
    {:db {::page :factories
          ::expanded-navbars #{}}
     :fx [[:dispatch [::game/fetch-game-data {:on-success [::install-ui]}]]]}))


(defn- run-validation
  [db event]
  (when-some [errors (db/db-errors db)]
    (js/console.warn errors)
    (js/console.warn "After " event)))


(defn- ^:dev/after-load install
  []
  (rdom/render [root] (js/document.getElementById "app")))


(rf/reg-fx
  ::install-ui
  (fn [_]
    (install)))


(defn- loading []
  [:section.section
   [:h1.title.has-text-centered "Loading..."]])


(defn init
  []
  (when ^boolean goog.DEBUG
    (rf/reg-global-interceptor (rf/after run-validation)))
  (rf/dispatch-sync [::init])

  (rdom/render [loading] (js/document.getElementById "app")))
  
  ;; (.addEventListener js/document "keydown" (fn [^js event]
  ;;                                            (let [e (or event js/window.event)]
  ;;                                              (when (= (.-keyCode e) 27)
  ;;                                                (rf/dispatch [::modal/hide])
  ;;                                                (rf/dispatch [::rs/hide-dropdown]))))))
