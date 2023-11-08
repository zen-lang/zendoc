(ns zd.core
  (:require
   [zen.core :as zen]
   [zd.store :as store]
   [zd.methods :as methods]
   [zen-web.core]
   [hiccup.core]
   [zd.git :as git]
   [clojure.walk]
   [ring.util.codec]
   [zd.view.core :as view])
  (:import [org.httpkit BytesInputStream]
           [org.eclipse.jgit.revwalk RevWalk]
           [org.eclipse.jgit.diff DiffFormatter]
           [java.io ByteArrayOutputStream]))


(defn form-decode [s]
  (clojure.walk/keywordize-keys (ring.util.codec/form-decode s)))

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
    {:status 200
     :body (hiccup.core/html (view/preview ztx req doc))}))

(defmethod zen/op 'zd/render-doc
  [ztx config {{id :id} :route-params :as req} & opts]
  (try
    (if-let [doc (store/doc-get ztx (symbol (or id "index")))]
      {:status 200
       :headers {"Cache-Control" "no-store, no-cache, must-revalidate, post-check=0, pre-check=0"}
       :body   (hiccup.core/html (view/page ztx req doc))}
      {:status 301
       :headers {"Location" (str "/new?docname=" id)
                 "Cache-Control" "no-store, no-cache, must-revalidate, post-check=0, pre-check=0"}})
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

;;TODO: move to store and write tests
(defn preview-doc [ztx doc]
  (let [errors  (store/doc-validate ztx doc)
        doc (cond-> doc (seq errors) (assoc :zd/errors errors))
        doc (if (:zd/subdocs doc)
              (-> doc (update :zd/subdocs (fn [subdocs]
                                            (->> subdocs
                                                 (mapv (fn [subdoc]
                                                         (let [errors  (store/doc-validate ztx subdoc)]
                                                           (cond-> subdoc (seq errors) (assoc :zd/errors errors)))))))))
              doc)]
    doc))

;; TODO: add validation and inference
(defmethod zen/op 'zd/render-preview
  [ztx _cfg {{id :id} :route-params body :body :as req} & opts]
  (let [docname (symbol id)
        content (if (=  BytesInputStream (type body)) (slurp body) body)
        doc     (store/to-doc ztx docname content {})
        doc (preview-doc ztx doc)]
    {:status 200
     :body (hiccup.core/html (view/preview ztx req doc))}))

(defmethod zen/op 'zd/new-doc
  [ztx _cfg {{docname :docname parent :parent} :params :as req} & opts]
  (let [doc {:zd/parent (when (some? parent)
                          (symbol parent))
             :zd/docname (cond docname (symbol docname)
                               parent (symbol (str parent ".<>"))
                               :else 'new)}]
    {:status 200
     :body (hiccup.core/html (view/editor ztx req doc ""))}))

(defmethod zen/op 'zd/new-preview
  [ztx _cfg {body :body :as req} & opts]
  (let [docname 'new
        content (if (=  BytesInputStream (type body)) (slurp body) body)
        doc     (store/to-doc ztx docname content {})
        doc (preview-doc ztx doc)]
    {:status 200
     :body (hiccup.core/html (view/preview ztx req doc))}))

(defmethod zen/op 'zd/create-doc
  [ztx _cfg { body :body session :session} & opts]
  (let [content (if (=  BytesInputStream (type body)) (slurp body) body)
        docname (store/extract-docname content)
        doc (store/file-save ztx docname content {:session session })]
    {:status 200
     :body (str "/" (:zd/docname doc))}))


(defmethod zen/op 'zd/save-doc
  [ztx _cfg {{id :id} :route-params body :body session :session :as req} & opts]
  (let [docname (symbol id)
        content (if (=  BytesInputStream (type body)) (slurp body) body)
        doc (store/file-save ztx docname content {:session session })]
    {:status 200
     :body (str "/" (:zd/docname doc))}))

(defmethod zen/op 'zd/check-errors
  [ztx _cfg req & opts]
  (if-let [errs  (seq (store/errors ztx))]
    {:status 200
     :body (str (count errs))}
    {:status 404
     :body "0"}))

(defmethod zen/op 'zd/doc-content
  [ztx config {{id :id} :route-params uri :uri hs :headers doc :doc :as req} & opts]
  (let [docname (symbol (or id "index"))
        doc (store/doc-get ztx docname)]
    {:status 200
     :headers {"Cache-Control" "no-store, no-cache, must-revalidate, post-check=0, pre-check=0"}
     :body  (hiccup.core/html (view/preview ztx req doc))}))

(defmethod zen/op 'zd/git-changes
  [ztx _cfg {{:keys [id]} :route-params :as req} & opts]
  (let [changes (git/changes ztx)
        history (git/history ztx 100)]
    {:status 200
     :body (hiccup.core/html (view/timeline ztx req {:changes changes :history history}))}))

(defmethod zen/op 'zd/git-commit
  [ztx _cfg {params :params :as req} & opts]
  {:status 200
   :body (hiccup.core/html [:pre (pr-str (form-decode (slurp (:body req))))])})

(defmethod zen/op 'zd/search-page
  [ztx _cfg {params :params :as req} & opts]
  (let [res (store/search ztx (:query params))]
    {:status 200
     :body (hiccup.core/html (view/search ztx req (assoc params :results res)))}))


(defmethod zen/op 'zd/render-widget
  [ztx _cfg {{id :id wgt :widget-id} :route-params r :root :as req} & opts]
  (let [doc (store/doc-get ztx (symbol id))]
    (if-not (nil? doc)
      {:status 200
       :body (hiccup.core/html (methods/widget ztx {:widget (keyword wgt) :root r :request req} doc))}
      {:status 200
       :body (hiccup.core/html [:div "Error: " id " is not found"])})))

(defmethod zen/op 'zd/delete-doc
  [ztx _cfg {{:keys [id]} :route-params session :session :as req} & opts]
  (let [docname (symbol id)]
    (store/file-delete ztx docname {:session session})
    {:status 200
     :body (str (store/parent-name docname))}))

(defmethod zen/op 'zd/re-validate
  [ztx _cfg _req & opts]
  (store/re-validate ztx)
  {:status 301
   :headers {"Location" "/errors"
             "Cache-Control" "no-store, no-cache, must-revalidate, post-check=0, pre-check=0"}})


(defmethod zen/start 'zd/zendoc
  [ztx config & opts]
  (println :zd/start config)
  (swap! ztx update :zd/dir (fn [x] (or x (:dir config) "docs")))
  (store/dir-load ztx (:dir config))
  config)

(defmethod zen/stop 'zd/zendoc
  [ztx config state]
  (println :zd/stop state)
  (swap! ztx dissoc :zdb :zd/backlinks))

(defmethod zen/op 'zd.events/logger
  [ztx config {ev-name :ev doc :params :as ev} & opts]
  (let [ev (assoc ev ::ts (str (java.util.Date.)))]
    (if (= ev-name 'zd.events/on-doc-load)
      (println (assoc (dissoc ev :params) :docname (:zd/docname doc)))
      (println ev))))

(defmethod zen-web.core/middleware-in 'zd/auth
  [ztx config ev & opts])

(defn start [& [dir gitsync]]
  (let [ztx (zen/new-context {:zd/dir dir :zd/gitsync gitsync})]
    (zen/read-ns ztx 'zd)
    (zen/start-system ztx 'zd/system)
    ztx))

(defn stop [ztx]
  (zen/stop-system ztx))

(defn -main [& opts]
  (start nil false))

(comment
  (def ztx (start nil true))

  (def ztx (start))

  (stop ztx)

  (:zd/props @ztx)

  (:zd/backlinks @ztx)

  (git/exec {:dir (:zd/dir @ztx) :exec ["git" "log" "-1"]})

  (git/exec {:dir (:zd/dir @ztx) :exec ["git" "diff" "--name-only" "4a569701e759120e8638eb46f8bedd4964b3250a"
                                        "7b7fa19bd92422ab9a2544599a1deb1360a19a25"]})

  (config ztx)
  (store/re-validate ztx)

  (edamame.core/parse-string "{%name name %type fhir/Patient}"))
