(ns refactory.app.pages.help
  (:require [clojure.string :as str]
            [re-frame.core :as rf]
            [refactory.app.pages :as pages]
            [refactory.app.ui :as ui]
            [refactory.app.util :refer [<sub]]))


(defmethod pages/page-config :help
  [_]
  {:enter [::init-ui]})


(defmulti section->title identity :default ::default)

(defmethod section->title ::default
  [section]
  (some-> section name str/capitalize))

(defmethod section->title :factories [_] "Design")


;;
;; Events
;;

(rf/reg-event-db
  ::init-ui
  (fn [db _]
    (assoc db ::ui {:section :overview})))


(rf/reg-event-db
  ::set-section
  (fn [db [_ section]]
    (assoc-in db [::ui :section] section)))


;;
;; Subscriptions
;;

(rf/reg-sub
  ::ui
  (fn [db _]
    (::ui db)))


(rf/reg-sub
  ::section
  :<- [::ui]
  (fn [ui _]
    (:section ui)))


;;
;; Components
;;

(defmulti section-content identity :default ::default)

(defmethod section-content ::default
  [_])


(defmethod section-content :overview
  [_]
  [:<>
   [:p.block
    "Welcome to Refactory, a respectful Satisfactory calculator."]

   [:p.block
    "I built this because I wanted a calclator that would leave me in complete
    control. This is not a “solver”. It will not plan factories or suggest
    optimized production flows. It's closer to a Satisfactory spreadsheet: you
    can pose what-if scenarios and it will calculate the results. It assumes
    that you know what you're doing or at least would like to."]

   [:p.block
    "At present, the two main features are called Design and Survey. Design is
    for modeling factories by choosing a set of recipes and configuring
    builders. Survey is for exploring the set of recipes reachable from a set
    of inputs. You can also configure the set of recipes that are currently
    available to you under Schematics. Use the menu to explore these topics in
    more detail."]

   [:h2.title.is-5 "Persistence"]
   [:p.block
    "Your state will be saved automatically to your browser's local storage
    every few seconds. No data is saved in the cloud, so if you reset your
    browser, the data will be lost. Using the Data menu, you can export your
    data any time, either for backup or to copy it to another device."]

   [:h2.title.is-5 "Device Support"]
   [:p.block
    "The smallest device targeted by this app is a tablet in landscape mode.
    Anything that's at least 1024px wide should be fine. Anything smaller will
    probably not be very usable."]

   [:h2.title.is-5 "App Limitations"]
   [:p.block
    "One of the basic rules of developing this app is that there is no manual
    data entry. Satisfactory kindly includes a catalog of items, recipes, and
    related information within the game bundle, in a file called Docs.json. All
    of the game data in Refactory comes from this file (along with extracted
    icons). When a new version of Satisfactory is released, the process of
    updating this data is more or less automated. I have no intention of adding
    features that would require manually gathering data from the in-game
    experience."]

   [:h2.title.is-5 "Project"]
   [:p.block
    "The source code is available at "
    [:a {:href "https://github.com/psagers/refactory"} "https://github.com/psagers/refactory"]
    ". The readme will have more information about the project status, but I'll
    reiterate here that this is a minor hobby project that may or may not be
    well maintained at any given time. If you find it useful, feel free to use
    it. If not, feel free to modify it for your own purposes or just ignore
    it."]])


(defmethod section-content :factories
  [_]
  [:<>
   [:p.block
    "This is the primary feature of Refactory and simply helps you model a
    prospective (or existing) factory. A factory consists of a collection of
    Jobs, each or which is defined by a recipe and a set of builders to
    manufacture it. You can't have more than one Job with the same recipe
    (there's no reason to), but you can configure each Job with any number of
    builders, including overdrive/underdrive settings."]

   [:h2.title.is-5 "Factories"]
   [:p.block
    "The first time you use this app, one of your first steps will be to create
    a new factory. You can keep as many factories as you wish; a dropdown in
    the upper right lets you switch between them."]

   [:p.block
    "Next to the factory name at the top of the page you'll find a dropdown
    with a gear icon ("
    [:span.icon.is-small [:i.bi-gear]]
    "). Use this to manage the factory settings and perform other factory-level
    operations."]

   [:p.block
    "Every factory can be in one of two modes: Continuous (the default) or
    Fixed. You can switch a factory between the two modes at any time without
    losing any data."]

   [:h3.title.is-6 "Continuous mode"]
   [:p.block
    "Continuous mode models a typical factory producing an unlimited number of
    items. All quantities are in units per minute."]

   [:p.block
    "In continuous mode, you can add and remove builders for each job. By
    expanding a job's builders, you can modify any of the overdrive settings.
    If any of a job's builders is not 100%, the builder count will be shown
    with an asterisk."]

   [:h3.title.is-6 "Fixed mode"]
   [:p.block
    "Fixed mode models a factory that's meant to produce a specific amount of
    some item. This is normally used to answer the question “How many units of
    A, B, and C do I need to produce a given number of X?” The obvious example
    is producing units of a project part you need to satisfy a milestone. In
    this case, all quantities are in absolute units."]

   [:p.block
    "In fixed mode, you don't manage the builders, you simply indicate how many
    times you intend to build this recipe. Note that you're not indicating the
    number of items you want: if a recipe produces 2 items each time and you
    want 1,000, you'll enter 500 for the build count."]

   [:h2.title.is-5 "Totals"]
   [:p.block
    "As you add and configure your jobs, a table on the right will add up all
    of the inputs and outputs of this factory. The totals are automatically
    divided into three categories: inputs, locals, and outputs."]

   [:ul.block.pl-4
    [:li.block
     [:b "Inputs"]
     " are items consumed but not produced by this factory. The assumption is
     that they are being sourced from a resource node or brought in by
     vehicle."]
    [:li.block
     [:b "Locals"]
     " are items both produced and consumed by this factory. If you're trying
     to consume more than you're producing, that may indicate a sub-optimal
     design and the shortfall will be rendered in red."]
    [:li.block
     [:b "Outputs"]
     " are items produced but not consumed by this factory. Presumably these
     are going to be stored for pickup or transported elsewhere."]]

   [:p.block
    "The totals table also has buttons that allow you to quickly add relevant
    recipes to the factory. It's often useful to start a new factory by adding
    a job for the final product and then using the "
    [:span.icon [:i.bi-plus-circle]]
    " buttons to add the intermediate recipes."]

   [:h2.title.is-5 "Special cases"]
   [:h3.title.is-6 "Ambiguous inputs"]
   [:p.block
    "When displaying the jobs in a factory, we attempt to analyze the
    relationships between jobs and arrange them such that any job consuming an
    item is shown after any jobs producing that item (a topological sort). For
    many factories, especially earlier in the game, this is straightforward.
    More complex, late-game recipes may produce waste products, some of which
    may be consumed by “earlier” jobs. This can make it impossible to
    automatically identify inputs as well as to sort the jobs in a
    deterministic order."]

   [:p.block
    "To get around this, there's a factory"
    [:span.icon [:i.bi-gear]]
    "option to manually identify one or more
    items as inputs to the factory. For instance, you might have a factory that
    extracts water from a local lake, but also includes a recipe that produces
    a small amount of water. You may need to add water as an explicit input to
    display everything in the most readable order."]

   [:p.block
    "Note that identyfing an input may change the order in which jobs and
    totals are displayed, but it will not affect any calculations. The totals
    are simple calculations on the inputs and outputs of jobs; the ordering is
    purely cosmetic. Any time ordering by dependencies is impossible or
    inappropriate, we'll fall back to ordering by the AWESOME Sink value of a
    recipe's outputs."]])


(defmethod section-content :survey
  [_]
  [:<>
   [:p.block
    "This is a secondary feature that can help explore the recipe catalog by
    the inputs you have available. The original intention was for considering
    the most appropriate factories for a given location based on the resources
    that are available locally (surveying the site, if you will)."]

   [:p.block
    "The survey page is fairly simple and does not support saving multiple
    sites. You simply select a set of items to consider as inputs and it will
    display all of the recipes that you could build. You can choose to either
    see recipes that only incorporate the given items (no imports) or recipes
    that incorporate all of the given items (perhaps with others)."]

   [:p.block
    "In either case, the recipes will be ordered by the number of “hops”
    required to reach them. Recipes that can built directly with the inputs are
    1 hop, recipes that can be built by including those outputs are 2 hops,
    etc."]

   [:p.block
    "Note that there are some inputs that will branch out into a huge number of
    recipes. For instance, any item that can be used to build empty canisters
    can “produce” any fluid that can be packaged. A certain amount of game
    knowledge and common sense is required to use this effectively."]])


(defmethod section-content :schematics
  [_]
  [:<>
   [:p.block
    "The schematics page allows you to indicate which recipes should be
    considered available in the rest of the app. Essentially, this allows you
    to mirror your current game state in terms of which tiers have been
    unlocked, which technologies researched, and which alternate recipes
    discovered."]

   [:p.block
    "All recipes are available by default, but they can be locked individually
    or in groups. In the case of milestones and research, it's fairly
    straightforward to check your game state and update the Refactory
    schematics to match. In the case of alternates, it's a more laborious
    process, but certainly doable. The search box in the upper right is
    particularly helpful here."]

   [:p.block
    "Locked recipes will not be offered when adding jobs to factories, nor
    listed in survey results. If you lock a recipe, it will not be removed from
    existing factories."]])


(defn- help-content
  []
  (let [section (<sub [::section])]
    [:section.section.pt-0
     [:h1.title (section->title section)]
     [section-content section]]))


(defn- menu-link
  [section]
  [:a {:class [(when (= section (<sub [::section])) "is-active")]
       :on-click (ui/link-dispatch [::set-section section])}
   (section->title section)])


(defn- main-menu
  []
  [:aside.menu
   [:ul.menu-list
    [:li [menu-link :overview]]
    [:p.menu-label "Features"]
    [:li [menu-link :factories]]
    [:li [menu-link :survey]]
    [:p.menu-label "Config"]
    [:li [menu-link :schematics]]]])


(defn root
  []
  [:div.columns
   [:div.column.is-narrow
    [:div.box
     [main-menu]]]
   [:div.column
    [help-content]]])
