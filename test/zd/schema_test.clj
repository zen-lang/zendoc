
(ns zd.schema-test
  (:require [zd.schema :as schema]
            [zen.core :as zen]
            [matcho.core :as matcho]
            [clojure.test :as t]))

;;TODO: add registry
;; (validate ztx 'schema-name data)

(t/deftest test-schema

  (def ztx (zen/new-context {}))

  (matcho/match
   (schema/validate ztx {:zd/require [:a :b]} {})
    [{:type :required, :message ":a is required", :path [:a]}
     {:type :required, :message ":b is required", :path [:b]}])

  (t/is (empty? (schema/validate ztx {:zd/require [:a :b]} {:a 1 :b 1})))

  (matcho/match
      (schema/validate ztx {:zd/props {:title {:zd/data-type 'zd.string}}} {:title 1})
    [{:type :type :message string?}])

  (t/is (empty? (schema/validate ztx  {:zd/props {:title {:zd/data-type 'zd.string}}} {:title "title"})))

  (matcho/match
      (schema/validate ztx {:zd/props {:value {:zd/data-type 'zd.number}}}
                       {:value "x"})
    [{:type :type :message string?}])

  (t/is (empty? (schema/validate ztx {:zd/props {:value {:zd/data-type 'zd.number}}} {:value 1})))
  (t/is (empty? (schema/validate ztx {:zd/props {:value {:zd/data-type 'zd.number}}} {:value 1.1})))

  (matcho/match
      (schema/validate ztx {:zd/props {:value {:zd/data-type 'zd.symbol}}}
                       {:value "x"})
    [{:type :type :message string?}])

  (t/is (empty? (schema/validate ztx {:zd/props {:value {:zd/data-type 'zd.symbol}}} {:value 'symbol})))

  (t/is (empty? (schema/validate ztx {:zd/props {:value {:zd/data-type 'zd.int}}} {:value 1})))
  (t/is (seq (schema/validate ztx {:zd/props {:value {:zd/data-type 'zd.int}}} {:value 10.1})))

  )
