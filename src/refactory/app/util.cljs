(ns refactory.app.util)


(defn per-minute
  ""
  [amount seconds]
  (-> (/ amount seconds)
      (* 60)
      (.toFixed 2)
      (js/parseFloat)))
