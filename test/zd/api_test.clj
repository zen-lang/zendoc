(ns zd.api-test
  (:require
   [zd.api]
   [clojure.string :as str]
   [matcho.core :as matcho]
   [clojure.test :refer [deftest is testing]]
   [zd.test-utils :as t]))



(def doc
  )

(deftest test-api

  (t/reset-project {})

  (testing "when document not found redirects to editor"
    (t/http-match
     {:uri "/testdoc"}
     {:status 301 :headers {"Location" "/testdoc/edit?"}}))

  (testing "editor config is rendered"
    (t/http-match
     {:uri "/testdoc/edit"}
     {:status 200}))

  (is (empty? (t/query '{:find [e] :where [[e :xt/id "'testdoc"]]})))

  ;; we are not throwing errors we save them
  (testing "saving document"
    (t/http-match
     {:uri "/testdoc/edit"
      :request-method :put
      :body (t/zd [:zd/docname 'testdoc]
                  [:title "testdoc"]
                  [:tags #{}]
                  [:desc "some"]
                  ['&subdoc1]
                  [:title "subdoc1"]
                  ['&subdoc2]
                  [:title "subdoc2"])}
     {:status 200})

    (matcho/match (t/get-doc 'testdoc) {:title "testdoc"})
    (matcho/match (t/get-doc 'testdoc.subdoc1) {:title "subdoc1"})
    (matcho/match (t/get-doc 'testdoc.subdoc2) {:title "subdoc2"})

    (t/query '{:find [e] :where [[e :xt/id id]]})

    (is (seq (t/query '{:find [e] :where [[e :xt/id "'testdoc"]]})))
    (is (seq (t/query '{:find [e] :where [[e :xt/id "'testdoc.subdoc1"]]})))
    (is (seq (t/query '{:find [e] :where [[e :xt/id "'testdoc.subdoc2"]]})))

    (t/http-match
     {:uri "/testdoc/edit"
      :request-method :put
      :body (t/zd [:zd/docname 'testdoc]
                  [:title "testdoc-change"]
                  [:tags #{}]
                  [:desc "some"]
                  ['&subdoc1]
                  [:title "subdoc1-change"]
                  ['&subdoc3]
                  [:title "subdoc3"])}
     {:status 200})


    (matcho/match (t/get-doc 'testdoc) {:title "testdoc-change"})
    (matcho/match (t/get-doc 'testdoc.subdoc1) {:title "subdoc1-change"})
    (matcho/match (is (nil? (t/get-doc 'testdoc.subdoc2))))
    (matcho/match (t/get-doc 'testdoc.subdoc3) {:title "subdoc3"})

    (t/query '{:find [p] :where
               [[e :xt/id "'testdoc"]
                [p :parent e]
                [p :zd/subdoc true]
                ]})


    (matcho/match
        (t/query '{:find [t] :where [[e :xt/id "'testdoc.subdoc1"] [e :title t]]})
      #{["subdoc1-change"]})

    (is (empty?
         (t/query '{:find [t] :where [[e :xt/id "'testdoc.subdoc2"] [e :title t]]})))

    (matcho/match
        (t/query '{:find [t] :where [[e :xt/id "'testdoc.subdoc3"] [e :title t]]})
      #{["subdoc3"]})

    )

  (testing "delete document"
    (t/http-match
     {:uri "/testdoc" :request-method :delete}
     {:status 200 :body #(not (nil? %))})

    (is (nil? (t/get-doc 'testdoc)))

    (is (empty? (t/query '{:find [e] :where [[e :xt/id "'testdoc"]]})))

    (t/http-match
     {:uri "/testdoc"}
     {:status 301 :headers {"Location" "/testdoc/edit?"}})


    (is (empty? (t/query '{:find [t] :where [[e :xt/id "'testdoc.subdoc1"] [e :title t]]})))
    (is (empty? (t/query '{:find [t] :where [[e :xt/id "'testdoc.subdoc3"] [e :title t]]})))
    )

  (testing "rename document"

    (t/http-match
     {:uri "/testdoc/edit"
      :request-method :put
      :body ":zd/docname testdoc\n:title \"testdoc\"\n:tags #{}\n:desc /"}
     {:status 200})

    (t/get-doc 'testdoc)

    (matcho/match (t/get-doc 'testdoc) {:title "testdoc"})


    (t/http-match
     {:uri "/testdoc/edit"
      :request-method :put
      :body ":zd/docname newname\n:title \"testdoc\"\n:tags #{}\n:desc /"}
     {:status 200})


    (is (nil? (t/get-doc 'testdoc)))
    (is (empty? (t/query '{:find [e] :where [[e :xt/id "'testdoc"]]})))

    (is (t/get-doc 'newname))
    (is (seq (t/query '{:find [e] :where [[e :xt/id "'newname"]]})))

    (t/http-match
     {:uri "/testdoc"}
     {:status 301
      :headers {"Location" "/testdoc/edit?"}})


    ))

(deftest document-rename)

;; (deftest document-rename

;;   (zen/stop-system ztx)

;;   (zen/read-ns ztx 'zd)

;;   (zen/read-ns ztx 'zd.test)

;;   (zen/start-system ztx 'zd.test/system)

;;   (def to-rename
;;     ;; TODO for some reason whitespace is required after :desc /\n
;;     ":zd/docname testdoc\n:title \"testdoc\"\n:tags #{}\n:desc /\n #testdoc2 link")

;;   (def to-link
;;     ":zd/docname testdoc2\n:title \"testdoc2\"\n:tags #{}\n:desc /")

;;   (matcho/assert
;;    {:status 200 :body string?}
;;    (web/handle ztx 'zd/api
;;                {:uri "/testdoc/edit"
;;                 :request-method :put
;;                 :body (t/req-body to-link)}))

;;   (is (seq (t/read-doc "testdoc2.zd")))

;;   (matcho/assert
;;    {:status 200 :body string?}
;;    (web/handle ztx 'zd/api
;;                {:uri "/testdoc/edit"
;;                 :request-method :put
;;                 :body (t/req-body to-rename)}))

;;   (is (not (str/blank? (t/read-doc "testdoc.zd"))))

;;   (def renamed
;;     (str to-rename "\n:zd/rename newdoc"))

;;   ;; old file deleted, new added
;;   ;; links are re calculated

;;   (matcho/assert
;;    {:status 200 :body "/newdoc"}
;;    (web/handle ztx 'zd/api
;;                {:uri "/testdoc/edit"
;;                 :request-method :put
;;                 :body (t/req-body renamed)}))

;;   ;; TODO think checkpoint api call
;;   (await fs/queue)
;;   (xtdb/sync (:node (d/get-state ztx)))

;;   (testing "on rename old file is deleted"
;;     (is (nil? (t/read-doc "testdoc.zd")))
;;     (is (not (str/blank? (t/read-doc "newdoc.zd")))))

;;   (testing "backlinks are reloaded"
;;     ;; TODO use designated api op
;;     (matcho/assert
;;      {:zd/meta {:backlinks #{'{:to testdoc2, :path [:desc], :doc newdoc}}}}
;;      (memstore/get-doc ztx 'testdoc2)))

;;   (testing "old file is removed from storage"
;;     ;; TODO use designated api op
;;     (is (empty? (zen/op-call ztx 'zd/query '{:find [?e]
;;                                              :where [[?e :xt/id ?id]
;;                                                      [(= ?id "testdoc")]]}))))
;;   ;; cleanup

;;   (matcho/assert
;;    {:status 200 :body string?}
;;    (web/handle ztx 'zd/api {:uri "/newdoc"
;;                             :request-method :delete}))

;;   ;; TODO for some reason root is nil
;;   (matcho/assert
;;    {:status 200 :body string?}
;;    (web/handle ztx 'zd/api {:uri "/testdoc2"
;;                             :request-method :delete}))

;;   (is (nil? (t/read-doc "testdoc2.zd")))
;;   (is (nil? (t/read-doc "newdoc.zd"))))
