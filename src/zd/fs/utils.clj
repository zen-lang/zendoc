(ns zd.fs.utils
  (:require [clojure.string :as str]))

(defn docpath [docname]
  (str (str/replace (str docname) "." "/") ".zd"))
