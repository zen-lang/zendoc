(ns zen-web.oauth.core
  (:require
   [zen-web.oauth.jwt :as jwt]
   [clojure.set :as set]
   [zen.core :as zen]
   [ring.util.codec]
   [clojure.string :as str]
   [cheshire.core :as json]
   [clj-http.client :as client]))

(defn decode64 [to-decode]
  (->> to-decode
       (.decode (java.util.Base64/getDecoder))
       String.))

(defn get-userinfo [{:keys [userinfo-endpoint user-email-endpoint org-endpoint]} token]
  (let [info
        (-> userinfo-endpoint
            (client/get {:accept :json
                         :headers {"Authorization" (str "Bearer " token)}
                         :throw-exceptions false})
            :body
            (json/parse-string keyword))

        email-info
        (when user-email-endpoint
          (-> user-email-endpoint
              (client/get {:accept :json
                           :throw-exceptions false
                           :headers {"Authorization" (str "Bearer " token)}})
              :body
              (json/parse-string keyword)))

        primary-email (->> email-info
                           (filter :primary)
                           first
                           :email)

        req-opts
        {:accept :json
         :throw-exceptions false
         :headers {"Authorization" (str "Bearer " token)}}

        user-orgs
        (when org-endpoint
          (->> (-> org-endpoint
                   (client/get req-opts)
                   :body
                   (json/parse-string keyword))
               (map :login)
               (into #{})))]

    (cond-> info
      (not-empty email-info)
      (assoc :email-info email-info)

      (and (not (:email info))
           primary-email)
      (assoc :email primary-email)

      org-endpoint
      (assoc :user-orgs user-orgs))))

(defn get-access-token
  "exchange auth code for access token"
  [base-uri code {:keys [id token-endpoint client-id client-secret]}]
  (let [params {:client_id client-id
                :client_secret client-secret
                :redirect_uri (str base-uri "/auth/callback/" id)
                :grant_type "authorization_code"
                :code code}

        req  {:accept :json
              :form-params params
              :throw-exceptions false
              :content-type :x-www-form-urlencoded}]

    ;; TODO remove clj-http
    (client/post token-endpoint req)))

(defn callback
  {:zen/tags #{'zen/op}}
  [ztx cfg {{:keys [provider-id]} :route-params
            {:keys [code state] :as qp} :query-params
            {:keys [providers organizations base-uri cookie secret]} :config
            :as req} & opts]
  (if (:error qp)
    {:status 403
     :body (merge {:message "auth callback error"} qp)}
    (let [provider (get providers provider-id)

          resp (get-access-token base-uri code provider)]

      (if (:error resp)
        {:status 403
         :body (merge {:message "access token req error"} resp)}

        (let [token (json/parse-string (:body resp) keyword)

              ;; get userinfo from provider api
              {:keys [user-orgs] :as userinfo}
              (get-userinfo provider (:access_token token))

              jwt (merge {:token token
                          :provider (select-keys provider [:id :system])
                          :userinfo userinfo}
                         (select-keys userinfo [:email :url :name]))]

          ;; check that user belongs to desired orgs if needed
          (if (or (nil? (:org-endpoint provider))
                  (not-empty (set/intersection (set organizations) user-orgs)))
            {:status 302
             :headers {"location" (decode64 state)}
             :cookies {cookie {:value (jwt/sign secret jwt :HS256)
                               :max-age 31536000
                               :path "/"}}}

            {:status 403
             :body (str "You should be member of [" (str/join "," organizations)
                        "] organizations. But only [" (str/join "," user-orgs) "]")}))))))

(defn redirect
  "redirect to ext provider initial oauth endpoint"
  {:zen/tags #{'zen/op}}
  [ztx cfg {{:keys [provider-id]} :route-params
            {:keys [state]} :query-params
            {:keys [providers base-uri]} :config} & opts]
  (if-let [{:keys [authorize-endpoint scopes client-id]} (get providers provider-id)]
    (let [params
          (cond-> {:response_type "code"
                   :scope (str/join " " scopes)
                   :client_id client-id
                   :redirect_uri (str base-uri "/auth/callback/" provider-id)}
            (not (nil? state)) (assoc :state state))]
      {:status 302
       :headers {"location" (str authorize-endpoint "?" (ring.util.codec/form-encode params))
                 "cache-control" "no-cache, no-store, max-age=0, must-revalidate"
                 "pragma" "no-cache"}})
    {:status 404
     :body {:message (str "provider " provider-id " not found")}}))

(defn encode64 [s]
  (when-not (str/blank? (str s))
    (.encodeToString (java.util.Base64/getEncoder) (if (string? s) (.getBytes s) s))))

(defn auth-redirect [{qs :query-string url :uri :as _req}]
  (let [state (encode64 (str url (when-not (empty? qs) (str "?" qs))))]
    {:status 302
     :headers {"location" (str "/auth?state=" state)
               "cache-control" "no-cache, no-store, max-age=0, must-revalidate"
               "pragma" "no-cache"}}))

(defn uri-split [uri]
  (->> (str/split uri #"/")
       (remove str/blank?)))

(defn match-uri
  "match left uri to right uri"
  [[fl & rl] [fr & rr]]
  (cond
    (and (nil? fl) fr) false
    (and (nil? fr) fl) false
    (and (nil? fl) (nil? fr)) true
    (and (= "*" fl) fr) true
    (= fl fr) (match-uri rl rr)))

(defn verify-jwt
  {:zen/tags #{'zen-web/middleware}}
  [ztx {config-sym :config} {:keys [cookies uri] :as req}]
  (let [{:keys [secret cookie public]} (zen/get-symbol ztx config-sym)
        public?
        (->> (into public ["/auth" "/auth/*"])
             (map #(uri-split %))
             (filter #(match-uri % (uri-split uri)))
             not-empty)]
    (when-not public?
      (if-let [token (get-in cookies [cookie :value])]
        ;; json parser may fail with an exception :(
        (let [{:keys [::response] :as jwt}
              (try (jwt/parse token)
                   (catch Exception ex
                     {::response {:status 403 :body (ex-message ex)}}))]
          (cond
            response {:zen-web.core/response response}
            (and jwt (jwt/verify jwt secret)) {:user (:claims jwt)}
            :else {:zen-web.core/response (auth-redirect req)}))
        {:zen-web.core/response (auth-redirect req)}))))

(defn snap-config
  "mount oauth configuration for handler ops"
  {:zen/tags #{'zen-web/middleware}}
  [ztx {config-sym :config} req]
  {:config (update (zen/get-symbol ztx config-sym)
                   :providers
                   (fn [provs]
                     (->> (map #(zen/get-symbol ztx %) provs)
                          (reduce (fn [acc p] (assoc acc (:id p) p)) {}))))})
