(ns zd.test-utils
  (:require
   [zen.core :as zen]
   [clojure.string :as str]
   [zen-web.core]
   [hickory.core]
   [matcho.core :as matcho]
   [clojure.java.io :as io]
   [clojure.walk]
   [zd.store :as store])
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




(defn rm-dir [dir]
  (when (Files/exists (path dir) (make-array java.nio.file.LinkOption 0))
    (let [dir (java.io.File. dir)]
      (doseq [f (->> (file-seq dir) (sort) (reverse))]
        (.delete f))
      (.delete dir))))

(defn clear-dir [dir]
  (rm-dir dir)
  (mk-dir dir))

(defn context [dir]
  (clear-dir dir)
  (zen/new-context {:zd/dir dir}))


(defn write-file
  ([ztx docname content]
   (let [dir (:zd/dir @ztx)
         file (store/docname-to-path docname)
         file-dir (store/parent-dir file)]
     (mk-dir (str dir "/" file-dir))
     (spit (str dir "/" file) content)))
  ([ztx content]
   (let [dir (:zd/dir @ztx)
         file (store/docname-to-path (:zd/docname content))
         file-dir (store/parent-dir file)]
     (mk-dir (str dir "/" file-dir))
     (spit (str dir "/" file)
           (->> (dissoc content :zd/docname)
                (mapv (fn [[k v]]
                        (str (pr-str k) " " (pr-str v))))
                (str/join "\n"))))))

(defn file-exists [ztx file]
  (let [dir (:zd/dir @ztx)]
    (.exists (io/file (str dir "/" file)))))

(defmacro doc? [ztx docname pat]
  `(let [doc# (store/doc-get ~ztx ~docname)]
     (matcho/match  doc# ~pat)
     doc#))

(defonce ctx (atom (zen/new-context)))

(defn reset-project [docs]
  (when-let [ztx @ctx]
    (zen/stop-system ztx))
  (rm-dir ".tmp")
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
;; (defn get-doc [s]
;;   (zd.memstore/get-doc @ctx s))

;; (defn all-errors []
;;   (zd.memstore/get-all-errors @ctx))

(defmacro match-doc [s pat]
  `(let [d# (get-doc ~s)]
    (matcho/match d# ~pat)
    d#))

(defn hiccup-find [body id]
  (let [res (atom [])]
    (clojure.walk/postwalk (fn [x]
                             (when (and (vector? x) (map? (second x)) (= id (:id (second x))))
                               (swap! res conj x))
                             x) body)
    @res))

(defn hiccup-text [body text]
  (let [res (atom [])]
    (clojure.walk/postwalk (fn [x]
                             (when (and (string? x) (= x text))
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

;; (defn query [q & params]
;;   (apply zd.datalog/query @ctx q params))

;; (defn datalog-save [data]
;;   (zd.datalog/save-doc )
;;   )


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
