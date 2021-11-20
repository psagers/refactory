(ns refactory.app.ui.forms
  (:require [re-frame.core :as rf]
            [refactory.app.db :as db]))




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
