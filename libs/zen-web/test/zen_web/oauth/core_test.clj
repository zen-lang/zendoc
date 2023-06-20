(ns zen-web.oauth.core-test
  (:require
   [zen.core :as zen]
   [zen-web.core :as http]
   [matcho.core :as matcho]
   [clojure.test :refer [deftest is testing]]))

(declare ztx)

(defn prepare! [])

(def pth (str (System/getProperty "user.dir") "/docs"))

(def system-ns
  '{:ns oauth.example
    :import #{zen-web zen-web.oauth}

    google
    {:zen/tags #{zen-web.oauth/provider}
     :id "google"
     :client-id "my-client-id"
     :client-secret "my-client-secret"
     :authorize-endpoint       "https://accounts.google.com/o/oauth2/v2/auth"
     :scopes         ["https://www.googleapis.com/auth/userinfo.profile"
                      "https://www.googleapis.com/auth/userinfo.email"]
     :userinfo-endpoint   "https://www.googleapis.com/oauth2/v1/userinfo"
     :token-endpoint      "https://www.googleapis.com/oauth2/v4/token"
     :display "Google"
     :system "https://google.com"}

    github
    {:zen/tags #{zen-web.oauth/provider}
     :id "github"
     :client-id "my-client-id-1"
     :client-secret "my-client-secret-1"
     :authorize-endpoint  "https://github.com/login/oauth/authorize"
     :scopes ["user" "read:org" "repo"]
     :display "Github"
     :system "https://github.com"
     :userinfo-endpoint   "https://api.github.com/user"
     :token-endpoint      "https://github.com/login/oauth/access_token"
     :org-endpoint "https://api.github.com/user/orgs"
     :user-email-endpoint "https://api.github.com/user/emails"}

    oauth-config
    {:zen/bind zen-web.oauth/config-binding
     :zen/tags #{zen-web.oauth/config}
     :providers [google github]
     :organizations ["my2ndOrg"]
     :base-uri "http://127.0.0.1.nip.io:8789"
     :cookie "token"
     :secret "secret-string"
     :public ["/public"]}

    simple-response
    {:zen/tags #{zen/op zen-web/op}
     :engine zen-web.engines/response
     :response {:status 200}}

    basic-auth
    {:zen/tags #{zen-web/middleware}
     :engine zen-web.engines/basic-auth
     :user "john"
     :password "milton"}

    api
    {:zen/tags #{zen-web/api}
     :engine zen-web/routemap
     :apis [zen-web.oauth/api]
     :mw [zen-web/defaults]
     "public" {:GET simple-response}
     "private" {:GET simple-response}}})

(defmethod zen/op 'zen-web.oauth/index
  [ztx cfg {{:keys [providers]} :config} & opts]
  {:status 200
   :body (keys providers)})

(defn prepare! []
  (def ztx (zen/new-context {:zd/paths [pth] :paths [pth]}))
  (zen/read-ns ztx 'zen-web)
  (zen/read-ns ztx 'zen-web.oauth)
  (zen/load-ns ztx system-ns))

(deftest config
  (prepare!)

  (is (empty? (zen/errors ztx))))

(deftest oauth-api
  ;; TODO how to test oauth callback?

  (prepare!)

  (testing "whitelist from oauth-config works"
    (matcho/assert
     {:status 200}
     (http/handle ztx 'oauth.example/api {:request-method :get :uri "/public"})))

  (testing "incorrect auth cookies"
    (matcho/assert
     {:status 302}
     (http/handle ztx 'oauth.example/api {:request-method :get :uri "/private"
                                          :headers {"cookie" "token=simple-string"}}))

    (matcho/assert
     {:status 403 :body #"Unexpected character"}
     (http/handle ztx 'oauth.example/api {:request-method :get :uri "/private"
                                          :headers {"cookie" "token=not.a.jwt"}})))

  (matcho/assert
   {:status 302
    :headers
    {"location" "/auth?state=L3ByaXZhdGU="
     "cache-control" "no-cache, no-store, max-age=0, must-revalidate"
     "pragma" "no-cache"}}
   (http/handle ztx 'oauth.example/api {:request-method :get :uri "/private"}))

  (testing "providers list is returned"
    (matcho/assert
    {:status 200
     :body ["google" "github"]}
    ;; TODO test that providers are rendered
    (http/handle ztx 'oauth.example/api {:request-method :get :uri "/auth"})))

  (testing "redirect is done"
    (matcho/assert
     {:status 404 :body {:message "provider not-found not found"}}
     (http/handle ztx 'oauth.example/api {:request-method :get :uri "/auth/not-found"}))

    (matcho/assert
     {:status 302 :headers {"location" #"accounts.google.com"}}
     (http/handle ztx 'oauth.example/api {:request-method :get :uri "/auth/google"})))

  (testing "callback returns error"
    (matcho/assert
     {:status 403,
      :body {:message "auth callback error"
             :error "redirect_uri_mismatch"}}
     (http/handle ztx 'oauth.example/api {:request-method :get
                                          :uri "/auth/callback/github"
                                          :query-string "error=redirect_uri_mismatch"}))))
