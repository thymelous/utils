(defproject backup-uploader "0.1.0"
  :description "Uploads Ruby-era FU backups (pg_dump and image files)"
  :url "http://github.com/thymelous/utils"
  :license {:name "CC0 1.0"
            :url "https://creativecommons.org/publicdomain/zero/1.0/legalcode"}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [org.clojure/core.match "0.3.0-alpha5"]
                 [clj-http "3.7.0"]
                 [hickory "0.7.1"]]
  :main ^:skip-aot backup-uploader.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
