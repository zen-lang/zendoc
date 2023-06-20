(ns zen-web.middlewares
  (:require
   [zen-web.utils :as utils]
   [clojure.string :as str]
   [ring.util.parsing :refer [re-token]]
   [clojure.walk :as walk]
   [hiccup.core :as hiccup]
   [ring.util.codec :as codec]
   [ring.middleware.cookies :as cookies])
  (:import java.util.Base64))

;; TODO impl middlewares, get rid of ring.* dependencies

(defn byte-transform [direction-fn string]
  (try
    (str/join (map char (direction-fn (.getBytes ^String string))))
    (catch Exception _)))

(defn decode-base64 [string]
  (byte-transform #(.decode (Base64/getDecoder) ^bytes %) string))

(defn basic-error [{:keys [request-method]}]
  {:zen-web.core/response
   (cond-> {:status 401
            :headers {"Content-Type" "text/plain"
                      "WWW-Authenticate" "Basic realm=restricted area"}}
     (not= request-method :head) (assoc :body "access denied"))})

(defn verify-basic-auth
  [ztx {:keys [user password]} req]
  (if-let [auth (get-in req [:headers "authorization"])]
    (let [cred (and auth (decode-base64 (last (re-find #"^Basic (.*)$" auth))))
          [u p] (and cred (str/split (str cred) #":" 2))]
      (if (and (= user u) (= password p))
        {:basic-authentication {:u user :p password}}
        (basic-error req)))
    (basic-error req)))

(defn set-cors-headers [ztx cfg req resp]
  (when-let [origin (get-in req [:headers "origin"])]
    (update resp :headers merge
            {"Access-Control-Allow-Origin" origin
             "Access-Control-Allow-Credentials" "true"
             "Access-Control-Expose-Headers" "Location, Content-Location, Category, Content-Type, X-total-count"})))

(defn parse-params* [s]
  (let [parsed
        (-> s
            ring.util.codec/form-decode
            walk/keywordize-keys)]
    (if (string? parsed)
      {(keyword parsed) nil}
      parsed)))

(defn parse-params [ztx cfg {rm :request-method hs :headers qs :query-string body :body :as req}]
  ;; TODO support non UTF-8 encodings
  (let [content-type (utils/content-type hs)

        form-params
        (when (and content-type
                   (str/starts-with? content-type "application/x-www-form-urlencoded")
                   body)
          (parse-params* (slurp body)))

        query-params
        (when qs
          (parse-params* qs))]

    {:params (merge form-params query-params)
     :query-params query-params
     :form-params form-params}))

(def re-cookie-octet #"[!#$%&'()*+\-./0-9:<=>?@A-Z\[\]\^_`a-z\{\|\}~]")

(def re-cookie-value (re-pattern (str "\"" re-cookie-octet "*\"|" re-cookie-octet "*")))

(def re-cookie (re-pattern (str "\\s*(" re-token ")=(" re-cookie-value ")\\s*[;,]?")))

(defn parse-cookies [ztx cfg {:keys [headers] :as req}]
  (when-let [cookie (get headers "cookie")]
    {:cookies
     (->> (for [[_ name value] (re-seq re-cookie cookie)]
            [name value])
          (map (fn [[name value]]
                 (when-let [value (codec/form-decode-str
                                   (str/replace value #"^\"|\"$" ""))]
                   [name {:value value}])))
          (remove nil?)
          (into {}))}))

(def attr-map {:domain "Domain", :max-age "Max-Age", :path "Path"
               :secure "Secure", :expires "Expires", :http-only "HttpOnly"
               :same-site "SameSite"})

(def same-site-map {:strict "Strict"
                    :lax "Lax"
                    :none "None"})

(defn write-attr-map [attrs]
  (for [[key value] attrs]
    (let [attr-name (name (get attr-map key))]
      (cond
        (satisfies? cookies/CookieInterval value) (str ";" attr-name "=" (cookies/->seconds value))
        (satisfies? cookies/CookieDateTime value) (str ";" attr-name "=" (cookies/rfc822-format value))
        (true? value)  (str ";" attr-name)
        (false? value) ""
        (= :same-site key) (str ";" attr-name "=" (get same-site-map value))
        :else (str ";" attr-name "=" value)))))

(defn set-cookies
  {:zen/tags #{'zen-web/middleware}}
  [ztx cfg req resp]
  (when-let [cookies (:cookies resp)]
    (let [http-cookies
          (->> cookies
               (map (fn [[k v]]
                      (if (map? v)
                        (apply str
                               (codec/form-encode {k (:value v)})
                               (write-attr-map (dissoc v :value)))
                        (codec/form-encode {k v})))))]
      (assoc-in resp [:headers "Set-Cookie"] (vec http-cookies)))))

(defn reduce-mw [apply-fn acc mws-in]
  (loop [mws mws-in
         acc acc]
    (if (empty? mws)
      acc
      (let [patch (apply-fn acc (first mws))]
        (if-let [resp (:zen-web.core/response patch)]
          resp
          (recur (rest mws)
                 (if (map? patch)
                   (utils/deep-merge acc patch)
                   acc)))))))

(defn all-of-in
  {:zen/tags #{'zen-web/middleware}}
  [ztx {:keys [mws]} req]
  (let [mws-in (->> mws
                    (map #(utils/resolve-mw ztx %))
                    (filter #(contains? (:dir %) :in)))
        apply-fn
        (fn [req* config]
          ((ns-resolve (find-ns 'zen-web.core) 'middleware-in) ztx config req*))]

    (reduce-mw apply-fn req mws-in)))

(defn all-of-out
  {:zen/tags #{'zen-web/middleware}}
  [ztx {:keys [mws]} req resp]
  (let [mws-out (->> mws
                     (map #(utils/resolve-mw ztx %))
                     (filter #(contains? (:dir %) :out)))
        apply-fn
        (fn [resp* config]
          ((ns-resolve (find-ns 'zen-web.core) 'middleware-out) ztx config req resp*))]

    (reduce-mw apply-fn resp mws-out)))

(defn one-of
  {:zen/tags #{'zen-web/middleware}}
  [ztx {:keys [mws]} req]
  (loop [mws (->> mws
                  (map #(utils/resolve-mw ztx %))
                  (filter #(contains? (:dir %) :in)))
         resp nil
         req req]
    (if (empty? mws)
      (if (nil? resp)
        {:status 403 :body {:message (str "one of mws " mws " must pass")}}
        resp)
      (let [patch ((ns-resolve (find-ns 'zen-web.core) 'middleware-in) ztx (first mws) req)]
        (if-let [resp (:zen-web.core/response patch)]
          (recur (rest mws)
                 resp
                 (utils/deep-merge req (dissoc patch :zen-web.core/response)))
          (if (map? patch)
            (utils/deep-merge req patch)))))))

(defn formats
  {:zen/tags #{'zen-web/middleware}}
  [ztx {:keys [formats]} req resp]
  (let [ct (get-in resp [:headers "Content-Type"])]
    (cond
      (and (contains? formats :html) ct (re-matches #"text/html" ct))
      {:headers
       {"Content-Type" "text/html; charset=utf-8"}
       :body (hiccup/html (:body resp))})))
