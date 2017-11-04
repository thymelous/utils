(ns backup-uploader.core
  (:gen-class)
  (:require [backup-uploader.dumploader :refer [read-images]]
            [backup-uploader.tagmapper :refer [translate-tag]]
            [backup-uploader.posthandler :refer [init-session post-image]]
            [clojure.core.match :refer [match]]))

(declare ask ask-password upload-after-id)

(defn -main [& args]
  (let [host (ask "Target host (e.g. localhost)")
        email (ask "Account email")
        pwd (ask-password "Account password")
        last-id (read-string (ask "Last uploaded # (0 to start from the beginning)"))]
    (upload-after-id last-id host email pwd)))

(defn ask [message]
  (println message)
  (read-line))

(defn ask-password [message]
  (println message)
  (println "(There's a problem in lein that results in System/console being nil,"
           "so the password needs to be entered in plain text."
           "Change it afterward or something.)")
  ; (.readPassword (. System console) ""))
  (read-line))

(defn load-images []
  (->>
    (read-images)
    (map (fn [image]
      (-> image
        (update :tags #(map translate-tag %))
        (update :tags #(match [%]
          [([artist & r] :seq)] (concat [artist "fandom: steven universe"] r))))))
    (sort-by #(read-string (:id %)))))

(defn upload-after-id [id host email pwd]
  (let [session (init-session host email pwd)
        image-list (drop-while #(>= id (read-string (:id %)))
                               (load-images))]
    (loop [[i & images-rest] image-list]
      (println "Uploading image #" (:id i))
      (let [upload-status (post-image host i session)]
        (match [upload-status]
          [302] (when-not (empty? images-rest)
                  (Thread/sleep 1000) ; let's not strain the server too much
                  (recur images-rest))
          [error] (println "Error when uploading, received" error))))))
