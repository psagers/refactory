(ns refactory.app.game
  "Managers the static game data loaded from the server."
  (:require [ajax.core :as ajax]
            [re-frame.core :as rf]
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

(defn input->recipes [item-id]
  (get-in @index [:input->recipes item-id]))

(defn output->recipes [item-id]
  (get-in @index [:output->recipes item-id]))


(defn -sorted-recipe-ids
  []
  (sort-by #((juxt :value :display) (id->recipe %))
           (keys (:id->recipe @index))))


(def sorted-recipe-ids (memoize -sorted-recipe-ids))


(comment
  (id->recipe "Recipe_Alternate_RecycledRubber_C"))


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
    {;; :db (assoc db ::index (index-game-data result))
     :fx [[::install-index (indexer/index-game-data result)]
          (when on-success
            [:dispatch on-success])]}))


(rf/reg-event-fx
  ::fetch-failed
  (fn [_ [_ {:keys [on-fail]} error]]
    (js/console.error error)
    {:fx [(when on-fail
            [:dispatch on-fail])]}))
