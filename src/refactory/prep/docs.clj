(ns refactory.prep.docs
  (:require [blancas.kern.core :as k]
            [clojure.data.json :as json]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [com.rpl.specter :as t]
            [meander.epsilon :as m]
            [refactory.prep.docs.parsers :as p]))


(defn- warn
  [& args]
  (binding [*out* *err*]
    (apply println args)))


(defn parse-int
  [x]
  (when (some? x)
    (Integer/parseInt x)))


(defn parse-double
  [x]
  (when (some? x)
    (Double/parseDouble x)))


(defn- ->description
  [value]
  (some-> value (str/replace "\r\n" "\n")))


(def ->form
  {"RF_SOLID" :solid
   "RF_LIQUID" :liquid
   "RF_GAS" :gas})


(defn- ->item-class
  [input]
  (when-not (empty? input)
    (let [{:keys [ok value] :as result} (k/parse p/class-name input)]
      (if ok
        value
        (warn "->item-class" (pr-str result))))))


(defn- ->icon
  [input]
  (when-not (empty? input)
    (let [{:keys [ok value] :as result} (k/parse p/icon input)]
      (if ok
        value
        (warn "->icon" (pr-str result))))))


(defn- ->list-of-class
  [input]
  (when-not (empty? input)
    (let [{:keys [ok value] :as result} (k/parse (p/list-of p/class-name) input)]
      (if ok
        value
        (warn "->list-of-class" (pr-str result))))))


(defn- ->list-of-dict
  [input]
  (when-not (empty? input)
    (let [{:keys [ok value] :as result} (k/parse (p/list-of p/dict) input)]
      (if ok
        value
        (warn "->list-of-dict" (pr-str result))))))


(defn- ->ingredients
  "Parses mIngredients and mProduct."
  [input]
  (some-> (->list-of-dict input)
          (m/search (m/scan {"ItemClass" (m/app ->item-class ?item-class)
                             "Amount" (m/app parse-int ?amount)})
                    {:item-id ?item-class
                     :amount ?amount})
          (vec)))


(defn- docs->builders
  "Extracts builders from Docs.json (constructor, assembler, etc.).

  Returns a collection of builders."
  [docs]
  (m/search
    docs

    (m/scan {"NativeClass" "Class'/Script/FactoryGame.FGBuildableManufacturer'"
             "Classes" (m/scan {"ClassName" ?class-name
                                "mDisplayName" ?display-name
                                "mDescription" (m/app ->description ?description)})})

    {:id ?class-name
     :display ?display-name
     :description ?description}))


(defn- docs->recipes
  "Extracs relevant recipes from Docs.json.

  builders comes from docs->builders. We only return recipes that can be built
  in one of the given builders."
  [docs builders]
  (let [builder-ids (set (map :id builders))]
    (m/search
      docs

      (m/scan {"Classes" (m/scan {"ClassName" ?class-name
                                  "mDisplayName" ?display-name
                                  "mIngredients" (m/some (m/app ->ingredients ?ingredients))
                                  "mProduct" (m/some (m/app ->ingredients ?products))
                                  "mManufactoringDuration" (m/app parse-double ?duration)
                                  "mProducedIn" (m/app #(some builder-ids (->list-of-class %))
                                                       (m/some ?builder-id))})})

      {:id ?class-name
       :display ?display-name
       :input ?ingredients
       :output ?products
       :duration ?duration
       :builder-id ?builder-id})))



(defn- recipes->item-ids
  "Returns the set of all item ids referenced by a collection of recipes."
  [recipes]
  (->> (m/search
         recipes
         (m/scan {:input [{:item-id !items} ...]
                  :output [{:item-id !items} ...]})
         !items)
       (into #{} cat)))


(defn- docs->items
  "Extracts relevant items from Docs.json.

  We only return items that are needed by one or more of the given recipes."
  [docs recipes]
  (let [item-ids (recipes->item-ids recipes)]
    (m/search
      docs

      (m/scan {"Classes"
               (m/scan {"ClassName" (m/app item-ids (m/some ?class-name))
                        "mDisplayName" ?display-name
                        "mDescription" (m/app ->description ?description)
                        "mForm" (m/app ->form (m/some ?form))
                        "mSmallIcon" (m/app ->icon ?icon)
                        "mResourceSinkPoints" (m/app parse-int ?sink-points)})})

      {:id ?class-name
       :display ?display-name
       :description ?description
       :form ?form
       :icon ?icon
       :value ?sink-points})))


(def ->schematic-type
  {"EST_Milestone" :milestone
   "EST_MAM" :research
   "EST_Alternate" :alternate
   "EST_ResourceSink" :shop})

   ;; "EST_Custom" :custom
   ;; "EST_HardDrive" :drive
   ;; "EST_Tutorial" :tutorial


(defn- docs->schematics
  "Extracts relevant schematics from Docs.json.

  We only return schematics that unlock one or more of the given recipes."
  [docs recipes]
  (let [recipe-ids (set (map :id recipes))]
    (m/search
      docs

      (m/scan {"Classes"
               (m/scan {"ClassName" ?class-name
                        "mDisplayName" ?full-name
                        "mDescription" ?description
                        "mType" (m/app ->schematic-type (m/some ?type))
                        "mTechTier" (m/app parse-int ?tier)
                        ;; "mSubCategories" (m/app ->list-of-class ?subcategories)
                        "mUnlocks" (m/scan {"Class" "BP_UnlockRecipe_C"
                                            "mRecipes" (m/app #(not-empty (filterv recipe-ids (->list-of-class %)))
                                                              (m/some ?recipes))})})})

      {:id ?class-name
       :display ?full-name
       :description ?description
       :kind ?type
       :tier ?tier
       ;; :subcategories ?subcategories
       :recipe-ids ?recipes})))


(defn- add-builders
  [{:keys [docs] :as game}]
  (assoc game :builders (docs->builders docs)))


(defn- add-recipes
  [{:keys [docs builders] :as game}]
  (assoc game :recipes (docs->recipes docs builders)))


(defn- add-items
  [{:keys [docs recipes] :as game}]
  (assoc game :items (docs->items docs recipes)))


(defn- add-schematics
  [{:keys [docs recipes] :as game}]
  (assoc game :schematics (docs->schematics docs recipes)))


(defn- adjust-liquid-amounts
  "Input and output amounts for liquids seem to be multiplied by 1000 in
  Docs.json."
  [{:keys [items] :as game}]
  (let [liquid-ids (set (m/search items (m/scan {:id ?id, :form (m/pred #{:liquid :gas})}) ?id))]
    (t/transform 
      [:recipes t/ALL (t/multi-path :input :output) t/ALL (t/pred (comp liquid-ids :item-id)) :amount]
      #(/ % 1000)
      game)))


(defn- mark-alternates
  [{:keys [schematics] :as game}]
  (let [alt-ids (->> (m/search schematics
                               (m/scan {:kind :alternate, :recipe-ids ?recipe-ids})
                               ?recipe-ids)
                     (into #{} cat))]
    (t/setval
      [:recipes t/ALL (t/pred (comp alt-ids :id)) :alternate]
      true
      game)))


(defn process
  "Parses Docs.json from a java.io.Reader and returns our final data structure."
  [reader]
  (-> {:docs (json/read reader)}
      add-builders
      add-recipes
      add-items
      add-schematics
      adjust-liquid-amounts
      mark-alternates
      (dissoc :docs)))


(comment
  (with-open [rdr (io/reader "docs.json")]
    (process rdr)))
