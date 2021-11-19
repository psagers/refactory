(ns refactory.app.ui.forms
  (:require [malli.core :as m]
            [malli.error :as me]
            [malli.transform :as mt]
            [re-frame.core :as rf]
            [refactory.app.db :as db]))


(defn- field-errors
  [attr value]
  (when-some [schema (db/attr->malli-schema attr)]
    (some-> (m/explain schema value)
            (me/humanize))))


(defn decode-value
  [attr value]
  (if-some [schema (db/attr->malli-schema attr)]
    (m/decode schema value mt/string-transformer)
    value))


(defn decode-values
  [values]
  (persistent!
    (reduce-kv (fn [m attr value]
                 (assoc! m attr (decode-value attr value)))
               (transient {})
               values)))


(defn values->errors
  "A Fork validation handler."
  [values]
  (persistent!
    (reduce-kv (fn [m attr value]
                 (let [value (decode-value attr value)]
                   (if-some [errors (field-errors attr value)]
                     (assoc! m attr errors)
                     m)))
               (transient {})
               values)))


(defn values-dispatch
  "Returns a function suitable for Fork's :on-submit option.

  At submit time, this will dispatch the given event with decoded field values
  appended."
  [event]
  (fn [{:keys [values]}]
    (rf/dispatch (conj event (decode-values values)))))
