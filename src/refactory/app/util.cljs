(ns refactory.app.util
  (:require-macros refactory.app.util))


(def number-formatter (js/Intl.NumberFormat.))

(defn num->str
  "Locale-aware number formatting."
  [n]
  (. number-formatter format n))


;; (defn with-places
;;   [value places]
;;   (js/parseFloat (.toFixed value places)))


(defn per-minute
  [amount seconds]
  (-> (/ amount seconds)
      (* 60)))
      ;; (with-places 2)))


(defn map-by
  [f coll]
  (into {} (map (juxt f identity)) coll))


(defn compare-by
  "Turns a key function into a comparator."
  [key-fn]
  (fn [a b]
    (compare (key-fn a) (key-fn b))))


(defn keep-vals
  "Transforms m by applying f to each value. Discards nils."
  [f m]
  (if (nil? m)
    {}
    (persistent! (reduce-kv (fn [m k v]
                              (if-some [v* (f v)]
                                (assoc! m k v*)
                                m))
                            (transient m)
                            m))))
