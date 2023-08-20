(ns zd.api.backlinks-test
  (:require
   [zd.test-utils :as tutils]
   [zd.fs :as fs]
   [zd.memstore :as memstore]
   [zen-web.core :as web]
   [matcho.core :as matcho]
   [clojure.set :as set]
   [zd.api]
   [clojure.test :refer [deftest is testing]]
   [zen.core :as zen]))

(defonce ztx (zen/new-context {}))

(deftest backlinks-update

  (tutils/prepare! ztx)

  (testing "two backlinks are present"
    (is (set/subset? #{{:to 'people.john :path [:founder] :doc 'customers.flame}
                       {:to 'people.john :path [:ceo] :doc 'customers.flame}}
                     (:backlinks (:zd/meta (memstore/get-doc ztx 'people.john))))))

  (testing "add new cust with a backlink"
    (def doc ":zd/docname customers.newcust\n:title \"my cust\"\n:desc \"\"\n:rel #{}\n:first-contact people.john")

    (matcho/assert
     {:status 200}
     (web/handle ztx 'zd/api
                 {:uri "/customers._draft/edit"
                  :request-method :put
                  :body (tutils/req-body doc)}))

    (is (string? (tutils/read-doc "customers/newcust.zd"))))

  ;; TODO think about awaits in zd.fs
  (await fs/queue)

  (testing "third backlink is added"
    (is (set/subset? #{{:to 'people.john :path [:founder] :doc 'customers.flame}
                       {:to 'people.john :path [:first-contact] :doc 'customers.newcust}
                       {:to 'people.john :path [:ceo] :doc 'customers.flame}}
                     (:backlinks (:zd/meta (memstore/get-doc ztx 'people.john))))))

  (testing "edit new cust backlink"
    (def doc ":zd/docname customers.newcust\n:title \"my cust\"\n:desc \"\"\n:rel #{}\n:discovered-by people.john")

    (matcho/assert
     {:status 200}
     (web/handle ztx 'zd/api
                 {:uri "/customers._draft/edit"
                  :request-method :put
                  :body (tutils/req-body doc)}))

    (await fs/queue)

    (is (string? (tutils/read-doc "customers/newcust.zd")))

    (is (not (contains? (:backlinks (:zd/meta (memstore/get-doc ztx 'people.john)))
                        {:to 'people.john :path [:first-contact] :doc 'customers.newcust})))

    (is (contains? (:backlinks (:zd/meta (memstore/get-doc ztx 'people.john)))
                   {:to 'people.john :path [:discovered-by] :doc 'customers.newcust})))

  (testing "third backlink is gone after newcust deletion"
      (matcho/assert
       {:status 200 :body string?}
       (web/handle ztx 'zd/api {:uri "/customers.newcust"
                                :request-method :delete}))

      (is (nil? (tutils/read-doc "testdoc.zd")))

      (is (not (contains? (:backlinks (:zd/meta (memstore/get-doc ztx 'people.john)))
                          {:to 'people.john :path [:first-contact] :doc 'customers.newcust})))))
