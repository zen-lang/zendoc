(ns zd.core
  (:require
   [zen.core :as zen]
   [zd.store :as store]
   [zd.methods :as methods]
   [zen-web.core]
   [hiccup.core]
   [zd.view.core :as view])
  (:import [org.httpkit BytesInputStream]))

(defn config [ztx]
  (zen/get-state ztx :zd/config))

{:zd/name '_errors
 :title "Errors"
 :zd/readonly true
 :zd/view [[:title] [:errors-view]]
 :errors-view true}


;; (defmethod web/middleware-out 'zd/layout
;;   [ztx config {page :page :as req} {bdy :body :as resp} & args]
;;   (when (and (not (string? bdy)) (= 200 (:status resp)))
;;     {:headers {"Content-Type" "text/html"}
;;      :body (layout/sidebar ztx {:request req} bdy)}))

(defmethod zen/op 'zd/preview-doc
  [ztx config {{id :id} :route-params uri :uri hs :headers doc :doc :as req} & opts]
  (let [docname (symbol (or id "index"))
        doc (store/doc-get ztx docname)]
    {:status 301
     :headers {"Location" (str "/" docname "/edit" "?" (:query-string req))
               "Cache-Control" "no-store, no-cache, must-revalidate, post-check=0, pre-check=0"}
     :body (hiccup.core/html (view/preview ztx req doc))}))

(defmethod zen/op 'zd/render-doc
  [ztx config {{id :id} :route-params uri :uri hs :headers doc :doc :as req} & opts]
  (try
    (let [docname (symbol (or id "index"))
          doc (store/doc-get ztx docname)]
      (if doc
        {:status 200
         :headers {"Cache-Control" "no-store, no-cache, must-revalidate, post-check=0, pre-check=0"}
         :body   (hiccup.core/html (view/page ztx req doc))}
        {:status 301
         :headers {"Location" (str "/" docname "/edit" "?" (:query-string req))
                   "Cache-Control" "no-store, no-cache, must-revalidate, post-check=0, pre-check=0"}
         :body "Editor"}))
    (catch Exception e
      {:status 500
       :headers {"Cache-Control" "no-store, no-cache, must-revalidate, post-check=0, pre-check=0"}
       :body   (hiccup.core/html [:pre (pr-str e)])})))

(defmethod zen/op 'zd/render-editor
  [ztx _cfg {{id :id} :route-params :as req} & opts]
  (let [docname (symbol (or id "index"))
        doc (store/doc-get ztx docname)
        content (store/file-content ztx docname)]
    {:status 200
     :body (hiccup.core/html (view/editor ztx req doc content))}))

;; TODO: add validation and inference
(defmethod zen/op 'zd/render-preview
  [ztx _cfg {{id :id} :route-params body :body :as req} & opts]
  (let [docname (symbol id)
        content (if (=  BytesInputStream (type body)) (slurp body) body)
        doc     (->> (store/to-docs ztx docname content {}) first)]
    {:status 200
     :body (hiccup.core/html (view/preview ztx req doc))}))

(defmethod zen/op 'zd/doc-content
  [ztx config {{id :id} :route-params uri :uri hs :headers doc :doc :as req} & opts]
  (let [docname (symbol (or id "index"))
        doc (store/doc-get ztx docname)]
    {:status 200
     :headers {"Cache-Control" "no-store, no-cache, must-revalidate, post-check=0, pre-check=0"}
     :body  (hiccup.core/html (view/preview ztx req doc))}))


(defmethod zen/op 'zd/render-widget
  [ztx _cfg {{id :id wgt :widget-id} :route-params r :root :keys [doc] :as req} & opts]
  (if-not (nil? doc)
    {:status 200
     :body (methods/widget ztx {:widget (keyword wgt) :root r :request req} doc)}
    {:status 200
     :body [:div "Error: " id " is not found"]}))

;; TODO handle rename
(defmethod zen/op 'zd/save-doc
  [ztx _cfg {{id :id} :route-params r :zd/root :as req} & opts]
  {:status 200 :body "TBD"})

(defmethod zen/op 'zd/delete-doc
  [ztx _cfg {{:keys [id]} :route-params :as req} & opts]
  {:status 200 :body "TBD"})


(defmethod zen/op 'zd.events/logger
  [ztx config {ev-name :ev :as ev} & opts]
  (println (assoc ev ::ts (str (java.util.Date.)))))

(defmethod zen/start 'zd/zendoc
  [ztx config & opts]
  (println :zd/start config)
  (store/dir-load ztx (:dir config))
  config)

(defmethod zen/stop 'zd/zendoc
  [ztx config state]
  (println :zd/stop state))

(comment

  (def ztx (zen/new-context {}))

  (zen/read-ns ztx 'zd)
  (zen/start-system ztx 'zd/system)
  (zen/stop-system ztx)

  (:zd/backlinks @ztx)

  (config ztx)

  )
