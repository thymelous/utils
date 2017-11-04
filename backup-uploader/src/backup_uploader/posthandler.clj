(ns backup-uploader.posthandler
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clj-http.client :as client]
            [clj-http.cookies :as cookies]
            [hickory.core :as html]
            [hickory.select :as dom]
            [clojure.java.io :as io]
            [clojure.core.match :refer [match]]))

(declare obtain-csrf-token)

(defn endpoint-for [host res]
  (str host "/" res))

(defn post-image [host image session]
  (let [tags (str/join "," (:tags image))
        source-url (:source-url image)
        image-file (io/file (:image-path image))
        csrf-token (obtain-csrf-token host session)]
    (:status
      (client/post (endpoint-for host "images")
        {:multipart
          [{:name "image[tags]" :content tags}
           {:name "image[source]" :content source-url}
           {:name "image[image]" :content image-file}
           {:name "_csrf_token" :content csrf-token}]
         :cookie-store session}))))

(defn init-session [host email pwd]
  (let [session (cookies/cookie-store)
        csrf-token (obtain-csrf-token host session)]
    (client/post (endpoint-for host "sign_in")
      {:form-params {"session[email]" email
                     "session[password]" pwd
                     "_csrf_token" csrf-token}
       :cookie-store session})
    session))

(defn obtain-csrf-token [host session]
  (->>
    (client/get (endpoint-for host "sign_in")
      {:cookie-store session})
    :body
    html/parse
    html/as-hickory
    (dom/select (dom/and
      (dom/tag :meta) (dom/attr :name #(= % "csrf-token"))))
    first
    :attrs
    :content))
