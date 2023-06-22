(ns zd.datalog-test
  (:require
   [zd.test-utils :as tutils]
   [zen-web.core :as web]
   [xtdb.api :as xtdb]
   [zd.api]
   [zd.datalog :as datalog]
   [clojure.test :refer [deftest is testing]]
   [zen.core :as zen]
   [matcho.core :as matcho]))

(defonce ztx (zen/new-context {}))

(comment
  (def ztx (zen/new-context {})))

(deftest datalog-engine
  (zen/stop-system ztx)

  (zen/read-ns ztx 'zd)

  (zen/read-ns ztx 'zd.test)

  (zen/start-system ztx 'zd.test/system)

  (xtdb/sync (:node (datalog/get-state ztx)))

  (testing "metadata is loaded into xtdb"
    (matcho/assert
     #{["customers"]}
     (datalog/query ztx '{:find [?id]
                          :where [[?e :meta/docname docname]
                                  [?e :xt/id ?id]]
                          :in [docname]}
                    'customers)))

  (matcho/assert
   #{["customers.flame"] ["customers._schema"]}
   (datalog/query ztx '{:find [e]
                        :where [[e :parent parent]]
                        :in [parent]}
                  'customers))

  (matcho/assert
   #{[{:xt/id "people.john"}]}
   (datalog/query ztx '{:find [(pull e [:xt/id :name])]
                        :where [[e :role "ceo"]]}))

  (zen/stop-system ztx))

(deftest xtdb-sync

  (zen/stop-system ztx)

  (zen/read-ns ztx 'zd)

  (zen/read-ns ztx 'zd.test)

  (zen/start-system ztx 'zd.test/system)

  (xtdb/sync (:node (datalog/get-state ztx)))

  (testing "add another person with role = ceo"
    (matcho/assert
     #{["people.john"]}
     (datalog/query ztx '{:find [?id]
                          :where [[?e :role "ceo"] [?e :xt/id ?id]]}))

    (def doc ":zd/docname people.bob\n:title \"Bob Barns\"\n:desc \"bob is the best\"\n:role #{\"ceo\"} ")

    (matcho/assert
     {:status 200}
     (web/handle ztx 'zd/api
                 {:uri "/people._draft/edit"
                  :request-method :put
                  :body (tutils/req-body doc)}))

    (is (tutils/read-doc "people/bob.zd"))
    (xtdb/sync (:node (datalog/get-state ztx)))

    (matcho/assert
     #{["people.john"] ["people.bob"]}
     (datalog/query ztx '{:find [?id]
                          :where [[?e :role "ceo"] [?e :xt/id ?id]]})))

  (testing "edit bob's role"
    (def doc ":zd/docname people.bob\n:title \"Bob Barns\"\n:desc \"bob is the best\"\n:role #{\"cpo\"} ")

    (matcho/assert
     {:status 200}
     (web/handle ztx 'zd/api
                 {:uri "/people.bob/edit"
                  :request-method :put
                  :body (tutils/req-body doc)}))

    (is (tutils/read-doc "people/bob.zd"))
    (xtdb/sync (:node (datalog/get-state ztx)))

    (matcho/assert
     #{["people.john"]}
     (datalog/query ztx '{:find [?id] :where [[?e :role "ceo"] [?e :xt/id ?id]]}))

    (matcho/assert
     #{["people.bob"]}
     (datalog/query ztx '{:find [?id] :where [[?e :role "cpo"] [?e :xt/id ?id]]})))

  (is (= 200 (:status (web/handle ztx 'zd/api {:uri "/people.bob" :request-method :delete}))))

  (is (nil? (tutils/read-doc "people/bob.zd")))

  (is (empty? (datalog/query ztx '{:find [?id] :where [[?e :role "cpo"] [?e :xt/id ?id]]})))

  (zen/stop-system ztx))

