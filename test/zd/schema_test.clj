
(ns zd.schema-test
  (:require [zd.schema :as schema]
            [zen.core :as zen]
            [matcho.core :as matcho]
            [clojure.test :as t]))

;;TODO: add registry
;; (validate ztx 'schema-name data)

(t/deftest test-schema

  (def ztx (zen/new-context {}))

  (schema/add-schema ztx 'req {:zd/require [:a :b]})

  (matcho/match
   (schema/validate ztx #{'req} {})
    [{:type :required, :message ":a is required", :path [:a]}
     {:type :required, :message ":b is required", :path [:b]}])

  (t/is (empty? (schema/validate ztx #{'req} {:a 1 :b 1})))


  (schema/add-schema ztx 'str {:zd/props {:title {:zd/data-type 'zd.string}}})

  (matcho/match
      (schema/validate ztx 'str  {:title 1})
    [{:type :type :message string?}])

  (t/is (empty? (schema/validate ztx  'str {:title "title"})))

  (schema/add-schema ztx 'num {:zd/props {:value {:zd/data-type 'zd.number}}})
  (matcho/match
      (schema/validate ztx 'num {:value "x"})
    [{:type :type :message string?}])

  (t/is (empty? (schema/validate ztx 'num {:value 1})))
  (t/is (empty? (schema/validate ztx 'num {:value 1.1})))


  (schema/add-schema ztx 'sym {:zd/props {:value {:zd/data-type 'zd.symbol}}})

  (swap! ztx assoc-in [:zdb 'symbol] {})
  (matcho/match
      (schema/validate ztx 'sym {:value "x"})
    [{:type :type :message string?}])

  (matcho/match
      (schema/validate ztx 'sym {:value 'wrong})
    [{:type :reference :message string?}])

  (t/is (empty? (schema/validate ztx 'sym {:value 'symbol})))

  (schema/add-schema ztx 'int {:zd/props {:value {:zd/data-type 'zd.int}}})
  (t/is (empty? (schema/validate ztx 'int {:value 1})))
  (t/is (seq (schema/validate ztx 'int {:value 10.1})))

  )
