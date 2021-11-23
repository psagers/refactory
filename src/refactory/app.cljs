(ns refactory.app
  (:require ["file-saver" :as file-saver]
            [day8.re-frame.http-fx]
            [re-frame.core :as rf]
            [reagent.core :as r]
            [reagent.dom :as rdom]
            [refactory.app.db :as db]
            [refactory.app.game :as game]
            [refactory.app.pages :as pages]
            [refactory.app.pages.factories :as factories]
            [refactory.app.pages.schematics :as schematics]
            [refactory.app.pages.survey :as survey]
            [refactory.app.persist :as persist]
            [refactory.app.ui :as ui]
            [refactory.app.ui.modal :as modal]))


;;
;; Active page
;;

(rf/reg-event-db
  ::set-page
  (fn [db [_ page]]
    (assoc db ::page page)))


(rf/reg-sub
  ::page
  (fn [db _]
    (::page db)))


(rf/reg-event-fx
  ::switch-to
  (fn [db [_ page]]
    (let [old (::page db)]
      {:fx [(when-some [leave (:leave (pages/page-config old))]
              [:dispatch leave])
            (when-some [enter (:enter (pages/page-config page))]
              [:dispatch enter])
            [:dispatch [::set-page page]]]})))


;;
;; Navbars
;;

(rf/reg-event-db
  ::toggle-navbar
  (fn [db [_ navbar-id]]
    (update db ::expanded-navbars #(if (% navbar-id) (disj % navbar-id) (conj % navbar-id)))))


(rf/reg-sub
  ::navbar-expanded?
  (fn [db [_ navbar-id]]
    (contains? (::expanded-navbars db) navbar-id)))


(defn navbar
  []
  (r/with-let [page (rf/subscribe [::page])
               expanded? (rf/subscribe [::navbar-expanded? :main])
               dirty? (rf/subscribe [::db/dirty?])]
    [:nav#main-navbar.navbar {:class [(if @dirty? "has-background-warning-light" "has-background-grey-lighter")]}
     [:div.navbar-brand
      [:div.navbar-item.has-background-dark.has-text-white-ter
       [:a.navbar-item.has-text-white {:href "/"} "Refactory"]]
      [:a.navbar-burger {:role "button"
                         :class [(when @expanded? "is-active")]
                         :on-click #(rf/dispatch [::toggle-navbar :main])}
       [:span] [:span] [:span]]]

     [:div.navbar-menu {:class [(when @expanded? "is-active")]}
      [:div.navbar-start
       [:a.navbar-item.is-tab {:class [(when (= @page :factories) "is-active")]
                               :on-click #(rf/dispatch [::switch-to :factories])}
        "Design"]
       [:a.navbar-item.is-tab {:class [(when (= @page :survey) "is-active")]
                               :on-click #(rf/dispatch [::switch-to :survey])}
        "Survey"]]

      [:div.navbar-end
       [:a.navbar-item.is-tab {:class [(when (= @page :schematics) "is-active")]
                               :on-click #(rf/dispatch [::switch-to :schematics])}
        "Schematics"]
       [:div.navbar-item.has-dropdown.is-hoverable
        [:a.navbar-link "Data"]
        [:div.navbar-dropdown.is-right
         [:a.navbar-item {:on-click (ui/link-dispatch [::persist/begin-export])}
          ;; [:span.icon [:i.bi-download]]
          [:span "Export data"]]
         [:a.navbar-item {:on-click (ui/link-dispatch [::persist/begin-import])}
          ;; [:span.icon [:i.bi-upload]]
          [:span "Import data"]]]]
       [:a.navbar-item.is-tab {:class [(when (= @page :help) "is-active")]
                               :on-click #(rf/dispatch [::switch-to :help])}
        [:i.bi-question-circle]]]]]))


;;
;; Root view
;;

(defn- nyi
  []
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
        :blank nil
        :factories [factories/root]
        :survey [survey/root]
        :schematics [schematics/root]
        [nyi])]

     (when @modal-opts
       [modal/modal @modal-opts])]))


;;
;; Initialization
;;

(defn- ^:dev/after-load install-ui
  []
  (rdom/render [root] (js/document.getElementById "app")))


(rf/reg-fx
  ::install-ui
  (fn [_]
    (install-ui)))


(rf/reg-event-fx
  ::install-ui
  (fn [_ _]
    {:fx [[::install-ui]
          [:dispatch [::switch-to :factories]]]}))


(rf/reg-event-fx
  ::init
  (fn [_ _]
    {:db {::page :blank
          ::expanded-navbars #{}}
     :fx [[:dispatch [::game/fetch-game-data {:on-success [::install-ui]}]]]}))


(defn- loading []
  [:section.section
   [:h1.title.has-text-centered "Loading..."]])


(defn init
  []
  (db/init)

  (rf/dispatch-sync [::init])
  (rdom/render [loading] (js/document.getElementById "app")))
