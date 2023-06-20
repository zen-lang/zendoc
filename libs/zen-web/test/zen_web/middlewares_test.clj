(ns zen-web.middlewares-test
  (:require
   [ring.util.codec :as codec :refer [form-encode]]
   [clojure.java.io :as io]
   [zen.core :as zen]
   [zen-web.core :as web]
   [matcho.core :as matcho]
   [clojure.test :refer [is deftest testing]]))

(def config
  '{:ns myweb
    :import #{zen-web}

    index-op
    {:zen/tags #{zen/op zen-web/op}
     :engine zen-web.engines/response
     :response {:status 200
                :body "Hello"}}

    admin-index-op
    {:zen/tags #{zen/op zen-web/op}
     :engine zen-web.engines/response
     :response {:status 200
                :body "Hello, admin"}}

    form-params
    {:zen/tags #{zen/op zen-web/op}
     :engine zen-web.engines/response
     :select :form-params
     :response {:status 200}}

    query-params
    {:zen/tags #{zen/op zen-web/op}
     :engine zen-web.engines/response
     :select :params
     :response {:status 200}}

    basic-auth
    {:zen/tags #{zen-web/middleware}
     :engine zen-web.engines/basic-auth
     :user "john"
     :password "123"}

    admin-api
    {:zen/tags #{zen-web/api}
     :engine zen-web/routemap
     :mw [basic-auth]
     :GET admin-index-op}

    cookies-op
    {:zen/tags #{zen/op zen-web/op}
     :engine zen-web.engines/response
     :select :cookies
     :response {:status 200}}

    *cookie-set
    {:zen/tags #{zen-web.engines/op zen/schema}
     :type zen/map
     :keys {:max-age {:type zen/integer}}}

    cookie-set
    {:zen/tags #{zen/op zen-web/op}
     :engine *cookie-set
     :max-age 1000}

    override-op
    {:zen/tags #{zen/op zen-web/op}
     :engine zen-web.engines/response
     :response {:status 200}}

    api
    {:zen/tags #{zen-web/api}
     :engine zen-web/routemap
     :mw [zen-web/cors]
     :GET index-op
     :POST zen-web/rpc
     "params-mw" {:mw [zen-web/parse-params]
                  :POST form-params
                  :GET query-params}
     "cookies-mw" {:mw [zen-web/cookies]
                   :GET cookies-op
                   "get-cookies" {:GET cookie-set}}
     "method-override" {:PUT override-op}
     "admin" {:apis [admin-api]}}

    http
    {:zen/tags #{zen/start zen-web/http}
     :engine zen-web/httpkit
     :port 8080
     :api api
     #_:formats #_#{zen-web/json zen-web/yaml zen-web/html}}

    system
    {:zen/tags #{zen/system}
     :start [http]}})

(def ztx (zen/new-context {}))

(zen/load-ns ztx config)

(deftest middleware-config
  (comment
    #_(zen/start-system ztx 'myweb/system)
    #_(zen/stop-system ztx))

  (zen/load-ns ztx config)

  (is (empty? (zen/errors ztx)))

  (is (= {:status 200, :body "Hello"}
         (web/handle ztx 'myweb/api {:uri "/" :request-method :get}))))

(deftest cors-middleware
  (def cors-headers {"access-control-request-headers" "Cookie, Content-Type"
                     "access-control-allow-methods" "*"
                     "origin" "localhost:8080"})

  (testing "options always returns cors headers"
    (matcho/match
     (web/handle ztx 'myweb/api {:uri "/" :request-method :options :headers cors-headers})
      {:status 200,
       :headers not-empty})

    (matcho/match
     (web/handle ztx 'myweb/api {:uri "/admin" :request-method :options :headers cors-headers})
      {:status 200,
       :headers not-empty}))

  (testing "if origin is provided cors headers are returned"
    (matcho/match
     (web/handle ztx 'myweb/api {:uri "/" :request-method :get :headers cors-headers})

      {:status 200
       :headers
       {"Access-Control-Allow-Origin" "localhost:8080"
        "Access-Control-Allow-Credentials" "true"
        "Access-Control-Expose-Headers"
        "Location, Content-Location, Category, Content-Type, X-total-count"}})))

(deftest basic-auth

  (matcho/assert
   {:status 401
    :headers {"Content-Type" "text/plain"
              "WWW-Authenticate" "Basic realm=restricted area"}
    :body "access denied"}
   (web/handle ztx 'myweb/api {:uri "/admin" :request-method :get}))

  (is (= {:status 200, :body "Hello, admin"}
         (web/handle ztx 'myweb/api {:uri "/admin"
                                     :request-method :get
                                     :headers {"authorization" "Basic am9objoxMjM="}}))))

(deftest cookies

  (defmethod zen/op 'myweb/*cookie-set
    [ztx {:keys [max-age]} req & opts]
    {:status 200
     :cookies {"token" {:value "justvalue"
                        :max-age max-age
                        :path "/"}
               "another-token" "another-value"}})

  (matcho/assert
   {:status 200
    :body {"USER_TOKEN" {:value "yes"}}}
   (web/handle ztx 'myweb/api {:uri "/cookies-mw"
                               :request-method :get
                               :headers {"cookie" "USER_TOKEN=yes"}}))

  (matcho/assert
   {:status 200
    :headers {"Set-Cookie" ["token=justvalue;Max-Age=1000;Path=/" "another-token=another-value"]}}
   (web/handle ztx 'myweb/api {:uri "/cookies-mw/get-cookies" :request-method :get})))

(deftest query-string
  (matcho/assert
   {:status 200 :body {:msg "hello love"}}
   (web/handle ztx 'myweb/api {:uri "/params-mw"
                               :request-method :get
                               :query-string "msg=hello+love"}))

  (matcho/assert
   {:status 200 :body {:msg nil}}
   (web/handle ztx 'myweb/api {:uri "/params-mw"
                               :request-method :get
                               :query-string "msg"}))

  (matcho/assert
   {:status 200
    :body {:value1 "a string", :value2 " yet another string %"}}
   (web/handle ztx 'myweb/api {:uri "/params-mw"
                               :request-method :post
                               :headers {"content-type" "application/x-www-form-urlencoded"}
                               :body (-> (str "value1=" (form-encode "a string")
                                              "&value2=" (form-encode " yet another string %"))
                                         .getBytes
                                         (io/input-stream))})))

(deftest combinator-all-of

  (def myconfig
    '{:ns mytest
      :import #{zen-web}

      index
      {:zen/tags #{zen/op zen-web/op}
       :engine zen-web.engines/response
       :select [:query-params :cookies]
       :response {:status 200
                  :cookies
                  {"token" {:value "justvalue"
                            :max-age "1000"
                            :path "/"}}}}

      api
      {:zen/tags #{zen-web/api}
       :engine zen-web/routemap
       :mw [zen-web/defaults]
       "index" {:GET index}}

      http
      {:zen/tags #{zen/start zen-web/http}
       :engine zen-web/httpkit
       :port 8080
       :api api}

      system
      {:zen/tags #{zen/system}
       :start [http]}})

  (zen/load-ns ztx myconfig)

  (is (empty? (zen/errors ztx)))

  (testing "params are parsed when all-of is applied"
    (matcho/assert
     {:status 200
      :body {:query-params {:key "value"}}}
     (web/handle ztx 'mytest/api {:request-method :get :uri "/index" :query-string "key=value"})))

  (testing "cookies are parsed when all-of is applied"
    (matcho/assert
     {:status 200
      :body {:cookies {"USER_TOKEN" {:value "yes"}}}}
     (web/handle ztx 'mytest/api {:uri "/index"
                                  :request-method :get
                                  :headers {"cookie" "USER_TOKEN=yes"}})))

  (testing "cookies are set when all-of is applied"
    (matcho/assert
     {:status 200
      :headers {"Set-Cookie" ["token=justvalue;Max-Age=1000;Path=/"]}
      :body empty?}
     (web/handle ztx 'mytest/api {:uri "/index"
                                  :request-method :get}))))
