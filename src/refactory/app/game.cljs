(ns refactory.app.game
  "Tools for working with the static game data."
  (:require [ajax.core :as ajax]
            [re-frame.core :as rf]
            [taoensso.encore :refer [keys-by]]))


;;
;; Game data
;;

(rf/reg-event-fx
  ::fetch-game-data
  (fn [_ _]
    {:fx [[:http-xhrio {:method :get
                        :uri "game.json"
                        :response-format (ajax/json-response-format {:keywords? true})
                        :on-success [::load-game]
                        :on-fail [::fetch-failed]}]]}))


(defn- ingredient-index
  [recipes ingr-key]
  (reduce (fn [m [item-id recipe-id]]
            (update m item-id (fnil conj #{}) recipe-id))
          {}
          (for [recipe recipes
                input (get recipe ingr-key)]
            [(:item-id input) (:id recipe)])))


(defn per-minute
  [amount seconds]
  (-> (/ amount seconds)
      (* 60)
      (.toFixed 2)
      (js/parseFloat)))


(defn- recipe->value
  "The normalized value of the output of a recipe.

  This is the AWESOME shop points per minute produced by this recipe. It's a
  useful heuristic for sorting recipes from basic to advanced."
  [id->item recipe]
  (letfn [(output->value [{:keys [item-id amount]}]
            (-> (id->item item-id)
                (:value)
                (* amount)))]
    (-> (transduce (map output->value) + 0 (:output recipe))
        (/ (:duration recipe))
        (* 60)
        (js/Math.round))))


(defn- annotated-recipes
  [game id->item]
  (for [recipe (:recipes game)]
    (assoc recipe :value (recipe->value id->item recipe))))


(defn- index-game-data
  [game]
  (let [id->builder (keys-by :id (:builders game))
        id->item (keys-by :id (:items game))
        id->recipe (keys-by :id (annotated-recipes game id->item))
        id->schematic (keys-by :id (:schematics game))
        input->recipes (ingredient-index (:recipes game) :input)
        output->recipes (ingredient-index (:recipes game) :output)]
    {:id->builder id->builder
     :id->item id->item
     :id->recipe id->recipe
     :id->schematic id->schematic
     :input->recipes input->recipes
     :output->recipes output->recipes}))


(rf/reg-event-db
  ::load-game
  (fn [db [_ result]]
    (assoc db ::index (index-game-data result))))


(rf/reg-event-fx
  ::fetch-failed
  (fn [_ [_ error]]
    (js/console.error error)))


;;
;; Subscriptions
;;

(rf/reg-sub
  ::index
  (fn [db _]
    (::index db)))


(rf/reg-sub
  ::recipes
  :<- [::index]
  (fn [index _]
    (sort-by (juxt :value :display) (-> index :id->recipe vals))))


(rf/reg-sub
  ::sorted-recipe-ids
  :<- [::index]
  (fn [{:keys [id->recipe]} _]
    (sort-by #((juxt :value :display) (id->recipe %))
             (keys id->recipe))))


(rf/reg-sub
  ::recipe-value
  :<- [::index]
  (fn [index [_ recipe-id]]
    (recipe->value index (get-in index [:id->recipe recipe-id]))))


(rf/reg-sub
  ::builder-by-id
  :<- [::index]
  (fn [{:keys [id->builder]} [_ builder-id]]
    (id->builder builder-id)))


(rf/reg-sub
  ::recipe-by-id
  :<- [::index]
  (fn [{:keys [id->recipe]} [_ recipe-id]]
    (id->recipe recipe-id)))


(rf/reg-sub
  ::item-by-id
  :<- [::index]
  (fn [{:keys [id->item]} [_ item-id]]
    (id->item item-id)))


(rf/reg-sub
  ::schematic-by-id
  :<- [::index]
  (fn [{:keys [id->schematic]} [_ schematic-id]]
    (id->schematic schematic-id)))


(rf/reg-sub
  ::made-with
  :<- [::index]
  (fn [{:keys [input->recipes]} [_ item-id]]
    (input->recipes item-id)))


(rf/reg-sub
  ::made-by
  :<- [::index]
  (fn [{:keys [output->recipes]} [_ item-id]]
    (output->recipes item-id)))
