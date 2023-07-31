(ns zd.api-test
  (:require
   [xtdb.api :as xtdb]
   [clojure.string :as str]
   [zd.datalog :as d]
   [zd.fs :as fs]
   [zd.memstore :as memstore]
   [zd.api]
   [matcho.core :as matcho]
   [clojure.test :refer [deftest is testing]]
   [zd.test-utils :as tutils]
   [zen.core :as zen]
   [zen-web.core :as web]))

;; TODO impl memstore -> str test util

(defonce ztx (zen/new-context {}))

(deftest create-delete

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

(deftest document-rename

  (zen/stop-system ztx)

  (zen/read-ns ztx 'zd)

  (zen/read-ns ztx 'zd.test)

  (zen/start-system ztx 'zd.test/system)

  (def to-rename
    ;; TODO for some reason whitespace is required after :desc /\n
    ":zd/docname testdoc\n:title \"testdoc\"\n:tags #{}\n:desc /\n #testdoc2 link")

  (def to-link
    ":zd/docname testdoc2\n:title \"testdoc2\"\n:tags #{}\n:desc /")

  (matcho/assert
   {:status 200 :body string?}
   (web/handle ztx 'zd/api
               {:uri "/testdoc/edit"
                :request-method :put
                :body (tutils/req-body to-link)}))

  (is (seq (tutils/read-doc "testdoc2.zd")))

  (matcho/assert
   {:status 200 :body string?}
   (web/handle ztx 'zd/api
               {:uri "/testdoc/edit"
                :request-method :put
                :body (tutils/req-body to-rename)}))

  (is (not (str/blank? (tutils/read-doc "testdoc.zd"))))

  (def renamed
    (str to-rename "\n:zd/rename newdoc"))

  ;; old file deleted, new added
  ;; links are re calculated

  (matcho/assert
   {:status 200 :body "/newdoc"}
   (web/handle ztx 'zd/api
               {:uri "/testdoc/edit"
                :request-method :put
                :body (tutils/req-body renamed)}))

  ;; TODO think checkpoint api call
  (await fs/queue)
  (xtdb/sync (:node (d/get-state ztx)))

  (testing "on rename old file is deleted"
    (is (nil? (tutils/read-doc "testdoc.zd")))
    (is (not (str/blank? (tutils/read-doc "newdoc.zd")))))

  (testing "backlinks are reloaded"
    ;; TODO use designated api op
    (matcho/assert
     {:zd/meta {:backlinks #{'{:to testdoc2, :path [:desc], :doc newdoc}}}}
     (memstore/get-doc ztx 'testdoc2)))

  (testing "old file is removed from storage"
    ;; TODO use designated api op
    (is (empty? (zen/op-call ztx 'zd/query '{:find [?e]
                                             :where [[?e :xt/id ?id]
                                                     [(= ?id "testdoc")]]}))))
  ;; cleanup

  (matcho/assert
   {:status 200 :body string?}
   (web/handle ztx 'zd/api {:uri "/newdoc"
                            :request-method :delete}))

  ;; TODO for some reason root is nil
  (matcho/assert
   {:status 200 :body string?}
   (web/handle ztx 'zd/api {:uri "/testdoc2"
                            :request-method :delete}))

  (is (nil? (tutils/read-doc "testdoc2.zd")))
  (is (nil? (tutils/read-doc "newdoc.zd"))))
