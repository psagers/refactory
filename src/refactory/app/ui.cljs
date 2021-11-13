(ns refactory.app.ui
  "Miscellaneous UI helpers.")
  ;; (:require [re-frame.core :as rf]
  ;;           [reagent.core :as r]
  ;;           [refactory.app.game :as game]))


;;
;; Icons
;;

(defn bi-icon
  "A bootstrap icon."
  ([icon-id]
   (bi-icon icon-id 16))
  ([icon-id size]
   [:img.bi-icon {:src (str "img/icons/bi/" icon-id ".svg")
                  :width size
                  :height size}]))

