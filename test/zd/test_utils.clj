(ns zd.test-utils
  (:require
   [zen.core :as zen]
   [clojure.string :as str]
   [zen-web.core]
   [hickory.core]
   [matcho.core :as matcho]
   [clojure.java.io :as io]
   [zd.memstore]
   [zd.datalog])
  (:import [java.nio.file Files Path]))

(def wd (System/getProperty "user.dir"))

(defn path [x]
  (Path/of (java.net.URI. (str "file:" wd "/" x))))


(defn req-body [s]
  (io/input-stream (.getBytes s)))

;; TODO use path prefix from test.edn
(defn read-doc [s]
  (let [f (io/file (str "customers-x/" s))]
    (when (.exists f)
      (slurp f))))

(def test-system

  '{:ns zd.test
    :import #{zd}

    zendoc
    {:zen/tags #{zd/zendoc}
     :paths [".tmp"]
     :root "index"}

    datalog
    {:zen/tags #{zen/start zd.engines/datalog}
     :engine zd.engines/datalog
     :zendoc zendoc}

    fs
    {:zen/tags #{zen/start zd.engines/fs}
     :engine zd.engines/fs
     :zendoc zendoc}

    system
    {:zen/tags #{zen/system}
     :start [datalog fs]}})

(defn mk-dir [dir]
  (Files/createDirectories (path dir) (make-array java.nio.file.attribute.FileAttribute 0)))

(defn mk-doc [docname content]
  (spit (str ".tmp/" (str/replace (str docname) #"\\." "/") ".zd") content))

(defonce ctx (atom (zen/new-context)))

(defn reset-project [docs]
  (when-let [ztx @ctx]
    (zen/stop-system ztx))
  (when (Files/exists (path ".tmp") (make-array java.nio.file.LinkOption 0))
    (let [dir (java.io.File. ".tmp")]
      (doseq [f (->> (file-seq dir) (sort) (reverse))]
        (.delete f))
      (.delete dir)))
  (mk-dir ".tmp")
  (doseq [[docname doc] docs]
    (mk-doc docname doc))
  (let [ztx (zen/new-context)]
    (reset! ctx ztx)
    (zen/load-ns ztx test-system)
    (zen/start-system ztx 'zd.test/system)))

(defn op [method params]
  (zen/op-call @ctx method params))

(defn http-get [uri & [params headers]]
  (let [resp (zen-web.core/dispatch @ctx 'zd/api {:hiccup true :request-method :get :uri uri})]
    (if (:body resp)
      (-> (hickory.core/as-hiccup (hickory.core/parse (:body resp)))
           (nth 0)
           (nth 3))
      resp)))

(defn get-symbol [s]
  (zen/get-symbol @ctx s))

;; TODO: this logic should live in codebase
(defn get-doc [s]
  (zd.memstore/get-doc @ctx s))

(defn hiccup-find [body id]
  (let [res (atom [])]
    (clojure.walk/postwalk (fn [x]
                             (when (and (vector? x) (map? (second x)) (= id (:id (second x))))
                               (swap! res conj x))
                             x) body)
    @res))

(defn http [req]
  (-> (zen-web.core/handle @ctx 'zd/api (update req :body (fn [x] (when x (req-body x)))))
      (update :body (fn [b]
                      (when b
                        (-> (hickory.core/as-hiccup (hickory.core/parse b))
                            (nth 0)
                            (nth 3)))))))

(defmacro http-match [req pat]
  `(let [resp# (http ~req)]
     (matcho/match
         resp# ~pat)
     resp#))

(defmacro hiccup-match [body id patt]
  `(let [res# (hiccup-find ~body ~id)]
     (matcho/match res# ~patt)
     res#))

(defn query [q & params]
  (apply zd.datalog/query @ctx q params))

(defn datalog-save [data]
  (zd.datalog/save-doc )
  )


(defn prepare! [ztx]
  (zen/stop-system ztx)

  (zen/read-ns ztx 'zd)

  (zen/read-ns ztx 'zd.test)

  (zen/start-system ztx 'zd.test/system))


(defn zd [ & xs]
  (->> xs
       (mapv (fn [x]
               (->> x  (mapv pr-str) (str/join " "))))
       (str/join "\n")))

(comment
  (reset-project {'person ":title \"Person\""})
  (get-doc 'person)
  (-> "person" http-get (hiccup-find "title"))

  (require 'hickory.core)
  



  )
