(ns refactory.app.ui.modal
  (:require [re-frame.core :as rf]))


(rf/reg-event-db
  ::show
  (fn [db [_ modal-id opts]]
    (assoc db ::modal (assoc opts ::modal-id modal-id))))


(rf/reg-event-db
  ::hide
  (fn [db [_ modal-id]]
    (let [{active-id ::modal-id} (::modal db)]
      (if (or (nil? modal-id) (= modal-id active-id))
        (dissoc db ::modal)
        db))))


(rf/reg-sub
  ::modal
  (fn [db _]
    (::modal db)))


(defmulti content ::modal-id)

(defmethod content :default
  [{::keys [modal-id]}]
  [:div.modal-content
   [:div.message.is-danger
    [:div.message-header
     [:p "Internal error"]
     [:button.delete {:on-click #(rf/dispatch [::hide modal-id])}]]
    "Modal ID " (pr-str modal-id) " is not implemented."]])


(defn modal
  [{::keys [modal-id close?] :as opts :or {close? true}}]
  [:div.modal.is-active
   [:div.modal-background]
   [content opts]
   (when close?
    [:button.modal-close.is-large {:on-click #(rf/dispatch [::hide modal-id])}])])


;; Standard confirmation modal
(defmethod content ::confirm-action
  [{::keys [modal-id], :keys [text button-label danger? on-confirm]}]
  [:div.modal-card
   [:header.modal-card-head {:class []}]
   [:section.modal-card-body
    [:p text]]
   [:footer.modal-card-foot.is-justify-content-end
    [:button.button {:on-click #(rf/dispatch [::hide modal-id])}
     "Cancel"]
    [:button.button {:class [(if danger? "is-danger" "is-success")]
                     :on-click #(do (rf/dispatch on-confirm)
                                    (rf/dispatch [::hide modal-id]))}
     button-label]]])
