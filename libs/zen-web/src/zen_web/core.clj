(ns zen-web.core
  (:require
   [ring.util.response :as mw-util]
   [clojure.java.io :as io]
   [zen.core :as zen]
   [ring.util.codec :as codec]
   [org.httpkit.server :as http-kit]
   [clojure.string :as str]
   [zen-web.oauth.core :as oauth]
   [zen-web.oauth.jwt :as jwt]
   [zen-web.utils :as utils]
   [zen-web.httpkit]
   [zen-web.routemap :as rm]
   [clojure.walk]
   [zen-web.middlewares :as mw]))


(defn form-decode [s] (clojure.walk/keywordize-keys (ring.util.codec/form-decode s)))

(defmulti middleware-in
  (fn [ztx cfg request]
    (zen/engine-or-name cfg)))

(defmulti middleware-out
  (fn [ztx cfg request response]
    (zen/engine-or-name cfg)))

(defmethod middleware-in 'zen-web.oauth/verify-jwt
  [ztx cfg req & args]
  (oauth/verify-jwt ztx cfg req))

(defmethod middleware-in 'zen-web.oauth/snap-config
  [ztx cfg req & args]
  (oauth/snap-config ztx cfg req))

(defmethod zen/op 'zen-web.oauth/test-token
  [ztx {config-sym :config} req & opts]
  (let [{:keys [secret cookie]} (zen/get-symbol ztx config-sym)]
    {:status 302
     :cookies
     {cookie
      {:value (jwt/sign secret {:token "test-token"} :HS256)
       :max-age 3153600
       :path "/"}}}))

(defmethod zen/op 'zen-web.oauth/redirect
  [ztx cfg req & opts]
  (oauth/redirect ztx cfg req))

(defmethod zen/op 'zen-web.oauth/callback
  [ztx cfg req & opts]
  (oauth/callback ztx cfg req))

(defmethod middleware-in 'zen-web.engines/basic-auth
  [ztx cfg req & args]
  (mw/verify-basic-auth ztx cfg req))

(defmethod middleware-out 'zen-web/cors
  [ztx cfg req resp & args]
  (mw/set-cors-headers ztx cfg req resp))

(defmethod middleware-in 'zen-web/parse-params
  [ztx cfg req & args]
  (mw/parse-params ztx cfg req))

(defmethod middleware-in 'zen-web/cookies
  [ztx cfg req & args]
  (mw/parse-cookies ztx cfg req))

(defmethod middleware-out 'zen-web/cookies
  [ztx cfg req resp & args]
  (mw/set-cookies ztx cfg req resp))

(defmethod middleware-out 'zen-web.engines/formats
  [ztx cfg req resp & args]
  (mw/formats ztx cfg req resp))

(defmethod middleware-in 'zen-web.engines/all-of
  [ztx cfg req & args]
  (mw/all-of-in ztx cfg req))

(defmethod middleware-out 'zen-web.engines/all-of
  [ztx cfg req resp & args]
  (mw/all-of-out ztx cfg req resp))

(defmethod middleware-in 'zen-web.engines/one-of
  [ztx cfg req & args]
  (mw/one-of ztx cfg req))

(defmulti resolve-route
  (fn [_ztx cfg _path {_params :params _mws :middlewares _pth :path}]
    (zen/engine-or-name cfg)))

;; TODO wrap in zen/op
(defmethod resolve-route 'zen-web/routemap
  [ztx cfg path ctx]
  (rm/resolve-route ztx cfg path ctx))

(defmethod resolve-route
  :default
  [ztx cfg _path ctx]
  (zen/error ztx 'zen-web/no-resolve-route-method {:method (zen/engine-or-name cfg) :path (:path ctx)})
  nil)

(def initial-ctx {:path [] :params {} :middlewares []})

(defn *resolve-route
  [ztx cfg-or-name path]
  (if (symbol? cfg-or-name)
    (let [cfg (zen/get-symbol ztx cfg-or-name)]
      (resolve-route ztx cfg path initial-ctx))
    (resolve-route ztx cfg-or-name path initial-ctx)))

(defmulti routes
  "collect routes"
  (fn [_ztx cfg _ctx] (zen/engine-or-name cfg)))

(defmethod routes
  :default
  [ztx cfg ctx]
  [(-> (assoc ctx :op 'unknown :error (str "method zen-web.methods/routes is not implemented for " (:zen/name cfg)))
       (update :path conj :?))])

(defmethod routes 'zen-web/routemap
  [ztx cfg ctx]
  (rm/*routes ztx cfg ctx))

(defn *routes
  "collect routes"
  [ztx cfg-or-name]
  (let [ctx {:path [] :middlewares [] :params #{} :by []}]
    (->>
     (if (symbol? cfg-or-name)
       (let [cfg (zen/get-symbol ztx cfg-or-name)]
         (routes ztx cfg ctx))
       (routes ztx cfg-or-name ctx))
     (sort-by (fn [x] (str/join "/" (:path x)))))))

;; TODO wrap in zen/op
(defn dispatch [ztx comp-symbol {uri :uri qs :query-string meth :request-method :as req}]
  ;; TODO we need zen/log or zen/event
  ;; (println meth uri (:query-string req))
  (let [api-config (zen/get-symbol ztx comp-symbol)
        path (conj (rm/pathify uri) (-> (or meth :get) name str/upper-case keyword))
        ;; TODO we need session param for zen/op?
        query-params (when qs (form-decode qs))
        session {}]
    (if-let [{op :op  params :params mw :middlewares}
             (resolve-route ztx api-config path initial-ctx)]
      (let [all-mws (map #(utils/resolve-mw ztx %) mw)
            ;; apply inbound middlewares
            req*
            (mw/reduce-mw (fn [req* config]
                            (middleware-in ztx config req*))
                          (assoc req :route-params params :params query-params)
                          (filter #(contains? (:dir %) :in)
                                  all-mws))

            ;; call the handler if needed
            resp
            (if (:status req*)
              req*
              (zen/op-call ztx op req* session))]

        ;; apply outbound middlewares
        (mw/reduce-mw (fn [resp* config]
                        (middleware-out ztx config req* resp*))
                      resp
                      (->> all-mws
                           (filter #(contains? (:dir %) :out))
                           reverse)))

      {:status 404 :body "route not found"})))

(defn handle [ztx api-symbol {:keys [request-method headers] :as request}]
  (let [method-override (and (= :post request-method) (get headers "x-http-method-override"))
        parsed-request
        (cond-> request
          :always (update :uri codec/url-decode)
          method-override (assoc :request-method (keyword (str/lower-case method-override))))]
    ;; TODO move this matching to cors mw sometimes, see Issues/1
    (if (= :options request-method)
      {:status 200
       :headers
       ;; TODO refer to https://developer.mozilla.org/en-US/docs/Web/HTTP/CORS
       ;; set headers accordingly
       {"Access-Control-Allow-Headers" (get headers "access-control-request-headers")
        "Access-Control-Allow-Methods" (get headers "access-control-request-method")
        "Access-Control-Allow-Origin" (get headers "origin")
        "Access-Control-Allow-Credentials" "true"
        "Access-Control-Expose-Headers"
        "Location, Transaction-Meta, Content-Location, Category, Content-Type, X-total-count"}}
      (dispatch ztx api-symbol parsed-request))))

(defmethod zen/start 'zen-web/httpkit
  [ztx config & opts]
  (let [web-config
        (merge {:worker-name-prefix "w"
                :thread 8
                :max-body 20971520}
               config)
        req-fn
        (fn [request] (handle ztx (:api config) request))]
    {:server (http-kit/run-server req-fn web-config)
     :config config}))

(defmethod zen/stop 'zen-web/httpkit
  [ztx config state]
  (let [srv (:server state)]
    (srv)))

(defn merge-unset [acc v]
  (if (map? acc)
    (utils/deep-merge acc v)
    v))

(defmethod zen/op 'zen-web.engines/response
  [ztx {:keys [select response] :as cfg} req & args]
  (cond-> response
    (vector? select)
    (update :body merge-unset (select-keys req select))

    (keyword? select)
    (update :body merge-unset (get req select))))

(defmethod zen/op 'zen-web.engines/redirect
  [ztx {:keys [to]} req & args]
  {:status 301
   :headers {"Location" to}})

(defmethod zen/op 'zen-web.engines/serve-static
  [ztx {:keys [serve]} {uri :uri rp :route-params :as req} & args]
  (let [file-path (str/join "/" (:* rp))]
    (if-let [f (or (io/resource file-path)
                   (->> serve
                        (map (fn [p]
                               (str (System/getProperty "user.dir") p)))
                        (map (fn [path]
                               (let [file-path* (str path "/" file-path)
                                     f (io/file file-path*)]
                                 (when (.exists f) f))))
                        (filter identity)
                        (first)))]
      ;; TODO get rid of mw util dep
      (mw-util/file-response (.getPath f))
      {:status 404
       :body "file not found"})))
