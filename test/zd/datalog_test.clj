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

(def ztx (zen/new-context {}))

(zen/read-ns ztx 'zd)
(zen/read-ns ztx 'zd.test)
(zen/start-system ztx 'zd.test/system)
(xtdb/sync (:node (datalog/get-state ztx)))

(comment
  (def ztx (zen/new-context {}))
  )

(deftest datalog-engine

  (zen/start-system ztx 'zd.test/system)
  (xtdb/sync (:node (datalog/get-state ztx)))

  (datalog/query ztx '{:find [?e]
                       :where [[?e :xt/id ?id]]})

  (datalog/query ztx '{:find [?e] :where [[?e :xt/id "'customers"]]})

  (testing "metadata is loaded into xtdb"
    (matcho/assert
     #{['customers]}
     (datalog/query ztx '{:find [?e] :where [[?e :xt/id "'customers"]]})))

  (datalog/query ztx '{:find [e]
                       :where [[e :parent "'customers"]]})

  (matcho/assert
   #{'[customers.partners-list] '[customers.flame] '[customers._schema]}
   (datalog/query ztx '{:find [e]
                        :where [[e :parent "'customers"]]}))

  (matcho/assert
   #{[#:xt{:id 'people.john}]}
   (datalog/query ztx '{:find [(pull e [:xt/id :name])]
                        :where [[e :role "ceo"]]}))

  )

(deftest xtdb-sync

  (zen/start-system ztx 'zd.test/system)

  (xtdb/sync (:node (datalog/get-state ztx)))

  (datalog/evict-by-query ztx '{:where [[e :xt/id "'people.bob"] ] :find [e]})

  (datalog/query ztx '{:where [[e :xt/id id]] :find [e]})

  (testing "add another person with role = ceo"
    (matcho/assert
     #{['people.john]}
     (datalog/query ztx '{:find [?id]
                          :where [[?e :role "ceo"] [?e :xt/id ?id]]})
     )

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
     #{['people.john] ['people.bob]}
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
     #{['people.john]}
     (datalog/query ztx '{:find [?id] :where [[?e :role "ceo"] [?e :xt/id ?id]]}))

    (matcho/assert
     #{['people.bob]}
     (datalog/query ztx '{:find [?id] :where [[?e :role "cpo"] [?e :xt/id ?id]]})))

  (is (= 200 (:status (web/handle ztx 'zd/api {:uri "/people.bob" :request-method :delete}))))

  (is (nil? (tutils/read-doc "people/bob.zd")))

  ;; (is (empty? (datalog/query ztx '{:find [?id] :where [[?e :role "cpo"] [?e :xt/id ?id]]})))

  )


(deftest datalog-sugar


  (def q
    "
e :parent #organizations
e :rel #rel.partner
p :organization e
p :role #roles.cto
> d
> e:xt/id
> e:rel
> (count e)
> (mean e)
")

  (def q2
    "
e :parent #customers
e :category cat
(clojure.string/starts-with? cat \"s\")
e :customer-since since
e :asc

> e:name
> (count e)
"
    )

  (def q3
    "
e :type #customers
> e
")

  (datalog/submit ztx {:xt/id "'i1" :type :type})
  (datalog/submit ztx {:xt/id "'i2" :type :type})

  (matcho/match
      (datalog/parse-query q3)
    '{:where [[e :type "'customers"]],
      :order []
      :find [e]})

  (def q
    "
e :title t
> e
> t
> e:title
"
    )


  (datalog/parse-query q)
  (datalog/sugar-query ztx q)


  )
