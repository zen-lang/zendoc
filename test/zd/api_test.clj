(ns zd.api-test
  (:require
   [clojure.string :as str]
   [zd.api]
   [matcho.core :as matcho]
   [clojure.test :refer [deftest is testing]]
   [zd.test-utils :as tutils]
   [zen.core :as zen]
   [zen-web.core :as web]))

(defonce ztx (zen/new-context {}))

(deftest crud-workflow
  "basic document creation flow works"

  (zen/stop-system ztx)

  (zen/read-ns ztx 'zd)

  (zen/read-ns ztx 'zd.test)

  (zen/start-system ztx 'zd.test/system)

  (testing "when document not found redirects to editor"
    (matcho/assert
     {:status 301 :headers {"Location" "/testdoc/edit?"}}
     (web/handle ztx 'zd/api {:uri "/testdoc"})))

  (testing "editor config is rendered"
    (matcho/assert
     {:status 200}
     (web/handle ztx 'zd/api {:uri "/testdoc/edit"})))

  (testing "saving document"
    (matcho/assert
     {:status 422 :body {:message string?}}
     (web/handle ztx 'zd/api
                 {:uri "/testdoc/edit"
                  :request-method :put
                  :body (tutils/req-body ":zd/docname testdoc._draft\n:desc /\n no docname present")}))

    (matcho/assert
     {:status 422 :body {:message string?}}
     (web/handle ztx 'zd/api
                 {:uri "/testdoc/edit"
                  :request-method :put
                  :body (tutils/req-body ":desc /\n no docname present")}))

    (matcho/assert
     {:status 200 :body string?}
     (web/handle ztx 'zd/api
                 {:uri "/testdoc/edit"
                  :request-method :put
                  :body (tutils/req-body ":zd/docname testdoc\n:title \"testdoc\"\n:tags #{}\n:desc /")}))

    (is (not (str/blank? (tutils/read-doc "testdoc.zd")))))

  (testing "delete document"
    (matcho/assert
     {:status 200 :body string?}
     (web/handle ztx 'zd/api {:uri "/testdoc" :request-method :delete}))

    (is (nil? (tutils/read-doc "testdoc.zd"))))

  (zen/stop-system ztx))
