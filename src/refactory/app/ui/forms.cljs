(ns refactory.app.ui.forms
  (:require [goog.functions :refer [debounce]]
            [reagent.core :as r]
            [reagent.ratom :as ratom]
            [re-frame.core :as rf]
            [refactory.app.db :as db]))


(defn search-field
  [{:keys [interval on-update] :or {interval 350}}]
  (r/with-let [text (ratom/atom "")
               update-now (fn [value] (rf/dispatch (conj on-update value)))
               update-soon (debounce update-now interval)]
    [:div.control.has-icons-right
     [:input.input.is-rounded {:type "text"
                               :placeholder "Search by name or output"
                               :autoFocus true
                               :value @text
                               :on-change #(let [value (-> % .-target .-value)]
                                             (reset! text value)
                                             (update-soon value))}]
     [:span.icon.is-right
      [:button.delete {:disabled (empty? @text)
                       :on-click #(do (reset! text "")
                                      (update-now ""))}]]]))


(defn decode-values
  "Decodes a map of attr->value with decode-value."
  [values opts]
  (reduce-kv (fn [m attr value]
               (assoc m attr (db/decode-value attr value opts)))
             {}
             values))


(defn on-submit
  "Returns a function suitable for Fork's :on-submit option.

  At submit time, this will dispatch the given event with decoded field values
  appended."
  [event]
  (fn [{:keys [values]}]
    (rf/dispatch (conj event (decode-values values {:default? true})))))
