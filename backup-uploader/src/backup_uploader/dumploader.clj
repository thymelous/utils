(ns backup-uploader.dumploader
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.core.match :refer [match]]))

(defrecord Image [id tags source-url image-path])

(declare image-lines
         read-image-line
         parse-tag-string
         find-image-file)

(defn read-images []
  (map read-image-line (image-lines)))

(defn image-lines []
  (with-open [dump-rdr (io/reader "dump.sql")]
    (doall
      (let [lines (line-seq dump-rdr)]
        (->> lines
          (drop-while #(not (str/starts-with? % "COPY images")))
          (rest)
          (take-while #(not (= % "\\."))))))))

(defn read-image-line [line]
  (match [(into [] (str/split line #"\t"))]
    [[id raw-tags _ _ _ _ _ _ source-url _ _ _ _ _]]
      (Image. id
              (parse-tag-string raw-tags)
              source-url
              (find-image-file id))))

(defn parse-tag-string [tag-string]
  (-> tag-string
    (str/replace #"\"" "")
    (str/replace #"^[{]" "")
    (str/replace #"[}]$" "")
    (str/split #",")))

(defn find-image-file [id]
  (first (.listFiles (io/file "images" id))))
