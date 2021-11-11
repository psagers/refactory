(ns refactory.prep
  (:require [cli-matic.core :refer [run-cmd]]
            [clojure.data.json :as json]
            [clojure.java.io :as io]
            [meander.epsilon :as m]
            [refactory.prep.docs :as docs])
  (:import [java.io BufferedReader BufferedWriter]))


(defn- run-game-data
  [{:keys [in out]}]
  (with-open [reader (if in (io/reader in) (BufferedReader. *in*))
              writer (if out (io/writer out) (BufferedWriter. *out*))]
    (json/write (docs/process reader) writer)))


(defn- run-icons
  [{:keys [in prefix rsync]}]
  (with-open [reader (io/reader in)]
    (let [game (json/read reader)]
      (doseq [path (-> game
                       (m/search {"items" (m/scan {"icon" ?path})} ?path)
                       (distinct))]
        (println (str (when rsync "+ ") prefix path)))
      (when rsync
        (println "+ */")
        (println "- *")))))


(def CONFIG
  {:command "prep"
   :description "Prepares Satisfactory assets for deployment."
   :subcommands [{:command "game-data"
                  :description "Parses relevant game data from Docs.json into our own structure."
                  :opts [{:as "Path to Docs.json"
                          :option "in"
                          :short 0
                          :type :string
                          :default "raw/docs.json"}
                         {:as "Path to an output file (normally game.json)"
                          :option "out"
                          :short 1
                          :type :string
                          :default "public/game.json"}]
                  :runs run-game-data}

                 {:command "icons"
                  :description "Prints a list of icon paths from game.json."
                  :opts [{:as "Path to game.json"
                          :option "in"
                          :short 0
                          :type :string
                          :default "public/game.json"}
                         {:as "Path prefix for output"
                          :option "prefix"
                          :short "p"
                          :type :string
                          :default ""}
                         {:as "Output rsync include rules"
                          :option "rsync"
                          :type :with-flag
                          :defalt false}]
                  :runs run-icons}]})


(defn -main
  [& args]
  (run-cmd args CONFIG))


(comment
  (with-open [docs (io/reader "docs.json")]
    (docs/parse docs)))
