(ns zen-web.core-test
  (:require
   [zen.core :as zen]
   [zen-web.core :as web]
   [matcho.core :as matcho]
   [clojure.test :refer [deftest is testing]]))

(def config
  '{:ns myweb
    :import #{zen-web}

    index
    {:zen/tags #{zen/op zen-web/op}
     :engine zen-web.engines/response
     :response {:status 200
                :body "Hello"}}

    override
    {:zen/tags #{zen/op zen-web/op}
     :engine zen-web.engines/response
     :response {:status 200}}

    basic-auth
    {:zen/tags #{zen-web/middleware}
     :engine zen-web.engines/basic-auth
     :user "john"
     :password "123"}

    serve-static
    {:zen/tags #{zen/op zen-web/op}
     :engine zen-web.engines/serve-static
     :serve ["/test/zen_web/static"]}

    api
    {:zen/tags #{zen-web/api}
     :engine zen-web/routemap
     "files" {:* {:GET serve-static}}
     :GET index
     "test-mw" {:mw [basic-auth]
                :GET index}
     "method-override" {:PUT override}}

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

(deftest config-test
  (comment
    #_(zen/start-system ztx 'myweb/system)
    #_(zen/stop-system ztx))

  (zen/load-ns ztx config)

  (is (empty? (zen/errors ztx))))

(deftest routes-test
  (matcho/match
   (web/*routes ztx 'myweb/api)
    [{:path [:GET]}
     {:path ["method-override" :PUT]}]))

(deftest execution-stack

  (testing "mw-in is executed after route is resolved"
    (is (= {:status 404, :body "route not found"}
           (web/handle ztx 'myweb/api {:uri "/not-found"
                                       :request-method :get
                                       :headers {"authorization" "wrong-auth"}})))

    (is (= {:status 200, :body "Hello"}
           (web/handle ztx 'myweb/api {:uri "/test-mw"
                                       :request-method :get
                                       :headers {"authorization" "Basic am9objoxMjM="}})))))

(deftest http-headers
  (testing "method override works"
    (matcho/assert
     {:status 200}
     (web/handle ztx 'myweb/api {:uri "/method-override"
                                 :request-method :post
                                 :headers {"x-http-method-override" "PUT"}}))))

(deftest serve-static

  ;; test file location
  ;; test/zen/http/static/content.txt

  (testing "file-path is resolved from :serve dir"
    (matcho/assert
     {:status 200
      :body #(instance? java.io.File %)}
     (web/handle ztx 'myweb/api {:uri "/files/content.txt" :request-method :get})))

  (testing "file-path is resolved from classpath"
    (matcho/assert
     {:status 200
      :body #(instance? java.io.File %)}
     (web/handle ztx 'myweb/api {:uri "/files/zen_web/static/content.txt" :request-method :get})))

  (matcho/assert
   {:status 404
    :body "file not found"}
   (web/handle ztx 'myweb/api {:uri "/files/not-found.jpg" :request-method :get})))

(deftest redirect-op

  (def myztx (zen/new-context))

  (def myapp-config
    '{:ns myapp
      :import #{zen-web}

      ->index
      {:zen/tags #{zen/op zen-web/op}
       :engine zen-web.engines/redirect
       :to "/index"}

      index
      {:zen/tags #{zen/op zen-web/op}
       :engine zen-web.engines/response
       :response {:status 200 :body "hello"}}

      api
      {:zen/tags #{zen-web/api}
       :engine zen-web/routemap
       :GET ->index
       "index" {:GET index}}

      http
      {:zen/tags #{zen/start zen-web/http}
       :engine zen-web/httpkit
       :port 5678
       :api api}

      system
      {:zen/tags #{zen/system}
       :start [http]}})

  (zen/load-ns myztx myapp-config)

  (comment
    (zen/start-system myztx 'myapp/system)

    (zen/stop-system myztx))

  (is (empty? (zen/errors myztx)))

  (matcho/assert
   {:status 301 :headers {"Location" "/index"}}
   (web/handle myztx 'myapp/api {:uri "/" :request-method :get})))

