
(ns zd.schema-test
  (:require [zd.schema :as schema]
            [zen.core :as zen]
            [matcho.core :as matcho]
            [clojure.test :as t]))

;;TODO: add registry
;; (validate ztx 'schema-name data)

(t/deftest test-schema

  (def ztx (zen/new-context {:zdb {'req {} 's.str {}}}))

  (schema/add-class ztx  {:zd/docname 'req :zd/require [:a :b]})

  (:zd/classes @ztx)
  (:zdb @ztx)


  (matcho/match
   (schema/validate ztx {:zd/type 'req})
    [{:type :required, :message ":a is required", :path [:a]}
     {:type :required, :message ":b is required", :path [:b]}])

  (matcho/match
      (schema/validate ztx {:zd/type #{'req}})
    [{:type :required, :message ":a is required", :path [:a]}
     {:type :required, :message ":b is required", :path [:b]}])

  (t/is (empty? (schema/validate ztx  {:zd/type #{'req} :a 1 :b 1})))

  (schema/add-prop ztx {:zd/docname 's.str :zd/type 'zd.prop :zd/data-type 'zd.string})

  (matcho/match
      (schema/validate ztx {:s/str 1})
    [{:type :type :message string?}])

  (t/is (empty? (schema/validate ztx {:s/str "str"})))

  (schema/add-prop ztx {:zd/docname 's.number :zd/data-type 'zd.number})
  (matcho/match
      (schema/validate ztx {:s/number "x"})
    [{:type :type :message string?}])

  (t/is (empty? (schema/validate ztx {:s/number 1})))
  (t/is (empty? (schema/validate ztx {:s/number 1.1})))


  (schema/add-prop ztx {:zd/docname 's.symbol :zd/data-type 'zd.symbol})

  (swap! ztx assoc-in [:zdb 'symbol] {})

  (matcho/match
      (schema/validate ztx {:s/symbol "x"})
    [{:type :type :message string?}])

  (matcho/match
      (schema/validate ztx  {:s/symbol 'wrong})
    [{:type :reference :message string?}])

  (t/is (empty? (schema/validate ztx {:s/symbol 'symbol})))


  (schema/add-prop ztx {:zd/docname 's.symbol-maybeset :zd/data-type 'zd.symbol :zd/maybe-set? true})

  (t/is (empty? (schema/validate ztx {:s/symbol-maybeset 'symbol})))
  (t/is (empty? (schema/validate ztx {:s/symbol-maybeset #{'symbol}})))



  (schema/add-prop ztx {:zd/docname 's.symbol-set :zd/data-type 'zd.symbol :zd/set? true})
  (t/is (not (empty? (schema/validate ztx {:s/symbol-set 'symbol}))))
  (t/is (empty? (schema/validate ztx {:s/symbol-set #{'symbol}})))


  (schema/add-prop ztx {:zd/docname 's.int :zd/data-type 'zd.int})

  (t/is (empty? (schema/validate ztx {:s/int 1})))
  (t/is (seq (schema/validate ztx {:s/int 10.1})))
  (t/is (seq (schema/validate ztx {:s/int ""})))

  (schema/add-class ztx {:zd/docname 'summary :zd/summary [:a :b]})

  (schema/summary ztx 'summary)

  (matcho/match
   (schema/summary ztx 'summary {:a 1 :b 2 :c 3})
   {:a 1, :b 2})


  (schema/add-prop ztx {:zd/docname '_.title :zd/data-type 'zd.string})
  (schema/get-prop ztx :title)

  (t/is (not (empty? (schema/validate ztx {:title 1}))))


  (schema/add-prop ztx {:zd/docname '_.date :zd/data-type 'zd.date})

  (t/is (not (empty? (schema/validate ztx {:date 1}))))
  (t/is (empty? (schema/validate ztx {:date #inst"2011"})))

  (schema/add-class ztx {:zd/docname 'org :zd/child-type 'org})

  (get-in @ztx [:zd/classes 'org :zd/child-type])

  (matcho/match
   (schema/infere ztx {:zd/docname 'org.o1 :zd/parent 'org :title "Org"})
  '{:zd/docname org.o1,
    :zd/type org,
    :zd/infered [:zd/type]})


  )
