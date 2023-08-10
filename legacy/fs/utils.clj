(ns zd.fs.utils
  (:require [clojure.string :as str]))

(defn docpath [docname]
  (str (str/replace (str docname) #"\." "/") ".zd"))

(defn name-to-dir [pths docname]
  (->> (str/split docname #"\.")
       butlast
       (str/join "/")
       (str (first pths) "/")))
