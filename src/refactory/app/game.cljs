(ns refactory.app.game
  "Managers the static game data loaded from the server."
  (:require [ajax.core :as ajax]
            [clojure.set :as set]
            [re-frame.core :as rf]
            [re-posh.core :as rp]
            [refactory.app.game.indexer :as indexer]))


(defonce index (atom nil))


;;
;; The global game data index should be accessed with these:
;;

(defn id->builder [builder-id]
  (get-in @index [:id->builder builder-id]))

(defn id->item [item-id]
  (get-in @index [:id->item item-id]))

(defn id->recipe [recipe-id]
  (get-in @index [:id->recipe recipe-id]))

(defn id->schematic [schematic-id]
  (get-in @index [:id->schematic schematic-id]))


;;
;; Schematic utils
;;

(defn schematics
  []
  (vals (@index :id->schematic)))


(defn schematic-ids
  []
  (keys (@index :id->schematic)))


;;
;; Recipe utils
;;

(def recipe-id-set (delay (-> @index :id->recipe keys set)))


(defn recipe-sort-key
  [recipe-id]
  ((juxt :value :display) (id->recipe recipe-id)))


(defn -sorted-recipe-ids
  []
  (sort-by recipe-sort-key (keys (:id->recipe @index))))


(def sorted-recipe-ids (memoize -sorted-recipe-ids))


(defn recipe-ids->output-ids
  "Given a collection of recipe-ids returns the item-ids of all outputs as a
  set."
  [recipe-ids]
  (into #{}
        (comp (mapcat (comp :output id->recipe))
              (map :item-id))
        recipe-ids))


(defn can-build-with?
  "True if the given recipe can be built with items that satisfy the predicate
  (usually a set of item-ids)."
  [item-id-pred recipe-id]
  (every? (comp item-id-pred :item-id)
          (-> recipe-id id->recipe :input)))


(defn item-id->deep-recipe-ids
  "Given an item-id, does a deep traversal of the recipe graph, finding every
  recipe that incorporates this item."
  [recipe-id-pool item-id]
  (loop [recipe-id-pool recipe-id-pool
         item-ids #{item-id}
         recipe-ids #{}]
    (let [new-recipe-ids (set/select #(some (comp item-ids :item-id)
                                            (-> % id->recipe :input))
                                     recipe-id-pool)]
      (if (empty? new-recipe-ids)
        recipe-ids
        (recur (set/difference recipe-id-pool new-recipe-ids)
               (set/union item-ids (recipe-ids->output-ids new-recipe-ids))
               (set/union recipe-ids new-recipe-ids))))))


(defn -base-recipe-ids
  "Recipe IDs not referenced by any schematic. These are always available."
  []
  (->> (keys (:id->recipe @index))
       (remove (set (mapcat :recipe-ids (schematics))))
       (set)))


(def base-recipe-ids (delay (-base-recipe-ids)))


(defn unlocked-recipe-ids
  [locked-schematic-ids]
  (let [unlocked-schematic-ids (remove (set locked-schematic-ids) (schematic-ids))
        additional-recipe-ids (mapcat (comp :recipe-ids id->schematic) unlocked-schematic-ids)]
    (set (concat @base-recipe-ids additional-recipe-ids))))


;;
;; Item utils
;;

(defn item-sort-key
  [item-id]
  ((juxt :value :display) (id->item item-id)))


;;
;; Generally useful subscriptions
;;


;; A sequence of schematic-ids that the player has not yet unlocked.
(rp/reg-query-sub
  ::locked-schematic-ids
  '[:find [?recipe-id ...]
    :where [?e :page/id :config]
           [?e :config/locked-schematic-ids ?recipe-id]])


;; A set of recipe-ids that are availble to the player. This includes those
;; unlocked by schematics and those that have no schematics (and thus are
;; assumed to be available from the outset).
(rf/reg-sub
  ::unlocked-recipe-ids
  :<- [::locked-schematic-ids]
  (fn [locked-schematic-ids _]
    (let [unlocked-schematic-ids (remove (set locked-schematic-ids) (schematic-ids))
          additional-recipe-ids (mapcat (comp :recipe-ids id->schematic) unlocked-schematic-ids)]
      (set (concat @base-recipe-ids additional-recipe-ids)))))


;; A set of item-ids that appear as inputs to unlocked recipes.
(rf/reg-sub
  ::unlocked-input-ids
  :<- [::unlocked-recipe-ids]
  (fn [recipe-ids _]
    (into #{}
          (comp (mapcat (comp :input id->recipe))
                (map :item-id))
          recipe-ids)))


;;
;; Game data initialization
;;

(rf/reg-event-fx
  ::fetch-game-data
  (fn [_ [_ opts]]
    {:fx [[:http-xhrio {:method :get
                        :uri "game.json"
                        :response-format (ajax/json-response-format {:keywords? true})
                        :on-success [::load-index opts]
                        :on-fail [::fetch-failed opts]}]]}))


(rf/reg-fx
  ::install-index
  (fn [new-index]
    (reset! index new-index)))


(rf/reg-event-fx
  ::load-index
  (fn [_ [_ {:keys [on-success]} result]]
    {:fx [[::install-index (indexer/index-game-data result)]
          (when on-success
            [:dispatch on-success])]}))


(rf/reg-event-fx
  ::fetch-failed
  (fn [_ [_ {:keys [on-fail]} error]]
    (js/console.error error)
    {:fx [(when on-fail
            [:dispatch on-fail])]}))
