(ns refactory.app.factories
  (:require [refactory.app.ui :as ui]))


(defn root []
  [:div
   [:div.is-flex.is-justify-content-center
    [ui/recipe-dropdown]]])
