(ns refactory.app.game.indexer
  "Indexes the static game data at load time."
  (:require [clojure.string :as str]
            [refactory.app.util :refer [map-by per-minute]]))


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
        (mapcat #(-> % :item-id id->item :search-terms) output)))


(defn- annotated-recipes
  [game id->item]
  (for [recipe (:recipes game)]
    (assoc recipe
           :value (recipe->value id->item recipe)
           :search-terms (recipe->search-terms id->item recipe))))


(defn- schematic->search-terms
  [id->recipe {:keys [display recipe-ids]}]
  (cons (str/lower-case display)
        (mapcat #(-> % id->recipe :search-terms) recipe-ids)))


(defn- annotated-schematics
  [game id->recipe]
  (for [schematic (:schematics game)]
    (assoc schematic
           :search-terms (schematic->search-terms id->recipe schematic))))


(defn- add-builders
  [index game]
  (assoc index :id->builder (map-by :id (:builders game))))


(defn- add-items
  [index game]
  (assoc index :id->item (map-by :id (annotated-items game))))


(defn- add-recipes
  [{:keys [id->item] :as index} game]
  (assoc index :id->recipe (map-by :id (annotated-recipes game id->item))))


(defn- add-schematics
  [{:keys [id->recipe] :as index} game]
  (assoc index :id->schematic (map-by :id (annotated-schematics game id->recipe))))


(defn index-game-data
  "Builds indexes based on the data from game.json."
  [game]
  (-> {}
      (add-builders game)
      (add-items game)
      (add-recipes game)
      (add-schematics game)))
