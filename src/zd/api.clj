(ns zd.api
  (:require
   [clojure.pprint :as pprint]
   [zd.layout :as layout]
   [zd.memstore :as memstore]
   [zd.meta :as meta]
   [zd.datalog]
   [hiccup.core :as hiccup]
   [zd.reader :as reader]
   [clojure.java.io :as io]
   [clojure.string :as str]
   [zen.core :as zen]
   [zd.fs :as fs]
   [zd.methods :as methods]
   [zd.render :as render]
   [zen-web.core :as web]
   [zd.datalog :as datalog]
   [zd.fs.utils :as futils]
   [zd.gitsync :as gitsync])
  (:import [java.io StringReader]))

;; TODO move to zen-web.http
(defn get-state [ztx]
  (->> [:zen/state :http :state]
       (get-in @ztx)))

(defn zendoc-config [ztx]
  (->> [:config :zendoc]
       (get-in (get-state ztx))
       (zen/get-symbol ztx)))

;; ISSUE mw does not work for / request
;; TODO remove this mw
(defmethod web/middleware-in 'zd/append-doc
  [ztx _cfg {{id :id} :route-params :as req} & opts]
  (when (some? id)
    ;; TODO make root required in render ops with zen sch
    {:zd/root (:root (zendoc-config ztx))
     :doc (memstore/get-doc ztx (symbol id))}))

(defmethod zen/op 'zd/render-doc
  [ztx config {{id :id} :route-params
               uri :uri
               hs :headers
               doc :doc :as req} & opts]
  (let [{r :root ps :paths :as config} (zendoc-config ztx)]
    (cond
      (= uri "/")
      {:status 301
       :headers {"Location" (str "/" r "?" (:query-string req))
                 "Cache-Control" "no-store, no-cache, must-revalidate, post-check=0, pre-check=0"}}

      (nil? doc)
      {:status 301
       :headers {"Location" (str "/" id "/edit" "?" (:query-string req))
                 "Cache-Control" "no-store, no-cache, must-revalidate, post-check=0, pre-check=0"}}

      (get hs "x-body")
      {:status 200
       :headers {"Cache-Control" "no-store, no-cache, must-revalidate, post-check=0, pre-check=0"}
       :body (hiccup/html (render/render-doc ztx {:request req :paths ps :doc doc :root r :config config} doc))}

      :else
      {:status 200
       :headers {"Cache-Control" "no-store, no-cache, must-revalidate, post-check=0, pre-check=0"}
       :body (render/doc-view ztx {:request req :paths ps :doc doc :root r :config config} doc)})))

(defmethod web/middleware-out 'zd/layout
  [ztx config {page :page :as req} {bdy :body :as resp} & args]
  (when (and (not (string? bdy)) (= 200 (:status resp)))
    {:headers {"Content-Type" "text/html"}
     :body (layout/sidebar ztx {:request req} bdy)}))

(defmethod zen/op 'zd/render-widget
  [ztx _cfg {{id :id wgt :widget-id} :route-params r :root :keys [doc] :as req} & opts]
  (if-not (nil? doc)
    {:status 200
     :body (methods/widget ztx {:widget (keyword wgt) :root r :request req} doc)}
    {:status 200
     :body [:div "Error: " id " is not found"]}))

(defn extract-id [lines]
  (-> (->> lines
           (filter #(or
                     (str/starts-with? % ":zd/rename") ;; OBSOLETE
                     (str/starts-with? % ":zd/docname")))
           (first))
      (str/trim)
      (str/split #"\s+")
      (second)))


(defn name-to-dir [pths docname]
  (->> (str/split docname #"\.")
       butlast
       (str/join "/")
       (str (first pths) "/")))

;; TODO remove all subdocs
;; (when rename
;;   (symbol? rename)
;;   (fs/delete-doc ztx docname)
;;   (datalog/evict ztx (str docname)))
;; (zen/pub ztx 'zd.events/fs-save {:docname docname :rename rename})
;; docname  (if (symbol? rename) (str rename) (str docname))

(defn delete-doc [ztx docname]
  (let [{pths :paths} (fs/get-state ztx)
        filepath      (str (first pths) "/" (futils/docpath docname))
        docname-sym (symbol docname)
        doc (memstore/get-doc ztx docname-sym)]
    (when-let [subdocs (:zd/subs doc)]
      (->> subdocs (mapv (fn [s] (delete-doc ztx s)))))
    (swap! ztx update :zdb dissoc docname-sym)
    (when (.exists (io/file filepath))
      (io/delete-file filepath))
    (datalog/evict ztx (str "'" docname))))

;; todo validate links
(defn save-doc [ztx docname content]
  (let [{root :root pths :paths} (fs/get-state ztx)
        docpath  (futils/docpath docname)
        filepath (str (first pths) "/" docpath)
        dirname  (futils/name-to-dir pths docname)
        resource-path docpath
        docname-sym (symbol docname)
        doc (memstore/get-doc ztx docname-sym)
        subdocs (:zd/subs doc)
        docs    (memstore/read-docs ztx {:path filepath :root root :resource-path resource-path :content content})
        docs-idx (group-by :zd/docname docs)]
    (.mkdirs (io/file dirname))
    (spit filepath content)
    (->> docs (mapv (fn [doc]
                      (memstore/put-doc ztx doc)
                      (let [idoc (memstore/infere-doc ztx (:zd/docname doc))]
                        (datalog/save-doc ztx idoc)))))
    (->> subdocs
         (mapv (fn [subdoc]
                 (when-not (contains? docs-idx subdoc)
                   (delete-doc ztx subdoc)))))
    (memstore/load-links! ztx)))


(defn get-lines [s]
  (->> s
       (StringReader.)
       (io/reader)
       (line-seq)))

(defn cleanup-doc [lines]
  (->> lines
       (remove #(str/starts-with? % ":zd/docname"))
       (remove #(str/starts-with? % ":zd/rename"))
       (str/join "\n")))

(defmethod zen/op 'zd/save-doc
  [ztx _cfg {{id :id} :route-params r :zd/root :as req} & opts]
  (let [lines     (get-lines (slurp (:body req)))
        content   (cleanup-doc lines)
        new-id    (extract-id lines)
        rename-to (when (and new-id (not (= new-id id))) (symbol new-id))]
    (if rename-to
      (do
        (delete-doc ztx id)
        (save-doc ztx new-id content))
      (save-doc ztx id content))
    {:status 200 :body (str "/" (or rename-to id))}))

(defn parent-link [id]
  (let [ parts (str/split id #"\.")]
    (if-let [parent (seq (butlast parts))]
      (str "/" (str/join "." parent))
      "/")))

(defmethod zen/op 'zd/delete-doc
  [ztx _cfg {{:keys [id]} :route-params :as req} & opts]
  (let [{r :root} (zendoc-config ztx)]
    (delete-doc ztx id)
    {:status 200 :body (parent-link id)}))

(defmethod zen/op 'zd/render-editor
  [ztx _cfg {{id :id} :route-params :as req} & opts]
  (let [doc (or (:doc req) {:zd/meta {:docname (symbol id)}})
        {r :root ps :paths :as config} (zendoc-config ztx)]
    {:status 200
     :body (render/editor ztx {:root r :paths ps :request req :doc doc :config config} doc)}))

(defmethod zen/op 'zd/render-preview
  [ztx _ {{id :id} :route-params :as req} & opts]
  (let [{r :root ps :paths :as config} (zendoc-config ztx)]
    {:headers {"Content-Type" "text/html"}
     :body (-> (render/preview ztx {:request req :paths ps :config config :root r} (slurp (:body req)))
               (hiccup/html))
     :status 200}))

(defmethod zen/op 'zd.events/logger
  [ztx config {ev-name :ev :as ev} & opts]
  ;; TODO filter out
  (when-not (or (= ev-name 'zd.events/on-doc-save)
                (= ev-name 'zd.events/on-doc-load))
    ;; TODO do not print large events
    (pprint/pprint (assoc ev ::ts (.toString (java.util.Date.))))))
