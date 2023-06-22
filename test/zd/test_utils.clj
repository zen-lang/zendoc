(ns zd.test-utils
  (:require
   [clojure.java.io :as io]))

(defn req-body [s]
  (io/input-stream (.getBytes s)))

;; TODO use path prefix from test.edn
(defn read-doc [s]
  (let [f (io/file (str "customers-x/" s))]
    (when (.exists f)
      (slurp f))))

