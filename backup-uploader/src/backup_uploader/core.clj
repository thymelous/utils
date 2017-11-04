(ns backup-uploader.core
  (:gen-class)
  (:require [backup-uploader.dumploader :refer [read-images]]
            [backup-uploader.tagmapper :refer [translate-tag]]
            [clojure.core.match :refer [match]]))
(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))

(defn load-images []
  (->>
    (read-images)
    (map (fn [image]
      (-> image
        (update :tags #(map translate-tag %))
        (update :tags #(match [%]
          [([artist & r] :seq)] (concat [artist "fandom: steven universe"] r))))))
    (sort-by #(read-string (:id %)))))
 
