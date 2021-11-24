(ns refactory.app.persist
  (:require ["file-saver" :as file-saver]
            [debux.cs.core :refer-macros [clog clogn]]
            [re-frame.core :as rf]
            [refactory.app.db :as db]
            [refactory.app.ui :as ui]
            [refactory.app.ui.modal :as modal]))


;;
;; Export
;;

(rf/reg-event-fx
  ::begin-export
  (fn [_ _]
    {:fx [[:dispatch [::modal/show ::export]]]}))


(rf/reg-event-fx
  ::end-export
  (fn [_ _]
    {:fx [[:dispatch [::modal/hide ::export]]]}))


(defn export-data!
  "Some browsers (iOS) are fussy about starting downloads from asynchronous
  events. We need to break the re-frame rules here and initiate the download
  directly from the event handler."
  []
  (-> (js/Blob. [(db/export-ds)] #js {:type "text/plain;charset=utf-8"})
      (file-saver/saveAs "refactory-export.txt")))


(defmethod modal/content ::export
  [_]
  [:div.modal-card
   [:header.modal-card-head
    [:p.modal-card-title "Export data"]
    [:button.delete {:on-click #(rf/dispatch [::end-export])}]]
   [:section.modal-card-body
    [:p "This will export your saved data to a file. You can transfer this to
        another machine or just back it up."]]
   [:footer.modal-card-foot.is-justify-content-flex-end
    [:button.button {:on-click (ui/link-dispatch [::end-export])}
     "Cancel"]
    [:button.button.is-success {:on-click #(do (export-data!)
                                               (rf/dispatch [::end-export]))}
     "Export"]]])


;;
;; Import
;;

(rf/reg-event-fx
  ::begin-import
  (fn [{:keys [db]} _]
    {:db (assoc db ::import {})
     :fx [[:dispatch [::modal/show ::import]]]}))


(rf/reg-event-fx
  ::end-import
  (fn [{:keys [db]} _]
    {:db (dissoc db ::import)
     :fx [[:dispatch [::modal/hide ::import]]]}))


(rf/reg-event-db
  ::import-files-changed
  (fn [db [_ files]]
    (if (= (.-length files) 1)
      (assoc-in db [::import :files] files)
      (update db ::import dissoc :files))))


(rf/reg-event-fx
  ::import-from-file
  (fn [{:keys [db]} _]
    (let [file (-> db ::import :files first)
          reader (js/FileReader.)]
      {:db (assoc-in db [::import :reader] reader)
       :fx [[::read-import-file {:reader reader, :file file}]]})))


(rf/reg-fx
  ::read-import-file
  (fn [{:keys [reader file]}]
    (set! (.-onload reader)
          #(rf/dispatch [::import-data-loaded (-> % .-target .-result)]))
    (.readAsText reader file)))


(rf/reg-event-fx
  ::import-data-loaded
  (fn [_ [_ data]]
    {:fx [[::import-data data]
          [:dispatch [::end-import]]]}))


(rf/reg-fx
  ::import-data
  (fn [data]
    (try
      (-> (db/import-ds data)
          (deref)
          (db/reset-conn!))
      (catch js/Error e
        (js/console.error e)))))


(rf/reg-sub
  ::import-state
  (fn [db _]
    (get db ::import)))


(rf/reg-sub
  ::import-files
  :<- [::import-state]
  (fn [state _]
    (get state :files)))


(rf/reg-sub
  ::import-reader
  :<- [::import-state]
  (fn [state _]
    (get state :reader)))


(defmethod modal/content ::import
  [_]
  (let [files @(rf/subscribe [::import-files])
        reader @(rf/subscribe [::import-reader])]
    [:div.modal-card
     [:header.modal-card-head
      [:p.modal-card-title "Import data"]
      [:button.delete {:on-click #(rf/dispatch [::end-import])}]]
     [:section.modal-card-body
      [:div.block.notification.is-danger
       "This will completely overwrite your current data."]
      [:p.block "Select a file that you previously exported from Refactory:"]
      [:div.block
       [:input {:type "file"
                :on-change #(rf/dispatch [::import-files-changed (-> % .-target .-files)])}]]]
     [:footer.modal-card-foot.is-justify-content-flex-end
      [:button.button {:on-click (ui/link-dispatch [::end-import])}
       "Cancel"]
      [:button.button.is-success {:class [(when reader "is-loading")]
                                  :disabled (nil? files)
                                  :on-click (ui/link-dispatch [::import-from-file])}
       "Import"]]]))
