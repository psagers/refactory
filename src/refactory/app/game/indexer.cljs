(ns refactory.app.game.indexer
  "Indexes the static game data at load time."
  (:require [clojure.string :as str]
            [refactory.app.util :refer [map-by per-minute]]))


;; (defn- ingredient-index
;;   [recipes ingr-key]
;;   (reduce (fn [m [item-id recipe-id]]
;;             (update m item-id conj-set recipe-id))
;;           {}
;;           (for [recipe recipes
;;                 input (get recipe ingr-key)]
;;             [(:item-id input) (:id recipe)])))


(defn- annotated-items
  [game]
  (for [{:keys [display] :as item} (:items game)]
    (assoc item :search-terms [(str/lower-case display)])))


(defn- recipe->io-value
  [id->item recipe output?]
  (letfn [(io->value [{:keys [item-id amount]}]
            (-> (id->item item-id)
                (:value)
                (* amount)))]
    (-> (transduce (map io->value) + (get recipe (if output? :output :input)))
        (per-minute (:duration recipe)))))


(defn- recipe->value
  "The normalized value of the output of a recipe.

  This is the AWESOME shop points per minute produced by this recipe. It's a
  useful heuristic for sorting recipes from basic to advanced.

  A few recipes have outputs without meaningful values, in which case we'll
  fall back to the value of the inputs."
  [id->item recipe]
  (let [output-value (recipe->io-value id->item recipe true)]
    (if (zero? output-value)
      (recipe->io-value id->item recipe false)
      output-value)))


(defn- recipe->search-terms
  [id->item {:keys [display output]}]
  (cons (str/lower-case display)
        (map #(-> % :item-id id->item :display str/lower-case) output)))


(defn- annotated-recipes
  [game id->item]
  (for [recipe (:recipes game)]
    (assoc recipe
           :value (recipe->value id->item recipe)
           :search-terms (recipe->search-terms id->item recipe))))


(defn index-game-data
  "Builds indexes based on the data from game.json."
  [data]
  (let [id->builder (map-by :id (:builders data))
        id->item (map-by :id (annotated-items data))
        id->recipe (map-by :id (annotated-recipes data id->item))
        id->schematic (map-by :id (:schematics data))]
        ;; input->recipes (ingredient-index (:recipes data) :input)
        ;; output->recipes (ingredient-index (:recipes data) :output)]
    {:id->builder id->builder
     :id->item id->item
     :id->recipe id->recipe
     :id->schematic id->schematic}))
     ;; :input->recipes input->recipes
     ;; :output->recipes output->recipes}))
