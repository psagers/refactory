(ns refactory.app.ui
  "Miscellaneous UI helpers."
  (:require [re-frame.core :as rf]))


(defn link-dispatch
  "A helper for on-click handlers on [:a] elements."
  [rf-event]
  (fn [^js event]
    (.preventDefault event)
    (rf/dispatch rf-event)))
