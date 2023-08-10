(ns zd.parser-test
  (:require
   [matcho.core :as matcho]
   [zen.core :as zen]
   [clojure.java.io :as io]
   [zd.parser :as parser]
   [clojure.test :refer [deftest is]]))

(def to-read (slurp (io/resource "zd/parser/example.zd")))

(deftest test-parser

  (def ztx (zen/new-context {}))

  (def result (parser/parse ztx 'mydoc to-read))

  (matcho/match
      result
    [{:zd/annotations nil?
      :zd/view
      [[:string {:type :edn}]
       [:symbol {:type :edn}]
       [:symbols {:type :edn}]
       [:int {:type :edn}]
       [:long {:type :edn}]
       [:date {:type :edn}]
       [:map {:type :edn}]
       [:vector {:type :edn}]
       [:zd/type {:type :edn}]
       [:multiline {:type :zentext, :multiline true}]
       [:textwithtitle {:type :zentext, :multiline true :title "Title"}]
       [:query {:type :?, :multiline true}]
       [:multiline-edn {:type :edn}]]
      :long 1.0,
      :int 1,
      :symbol 'symbol,
      :symbols #{'symbols}
      :multiline "String 1\nString 2\n",
      :string "Title",
      :vector [1 2 3 4],
      :map {:a 1, :b 2},
      :date inst?
      :query "e :zd/type zd.class\n> e\n"
      :multiline-edn {:a 1 :b 2 :c 3}}
     {:zd/view [[:title {:type :edn}]]
      :zd/name "subdoc-1"
      :title "S1"
      :local-ref 'mydoc}
     {:zd/view [[:title {:type :edn}]]
      :zd/name "subdoc-2"
      :title "S2"}
     {:zd/docname 'mydoc.doc-3,
      :zd/subdoc? true,
      :title "Anonimous1"}
     {:zd/view [[:title {:type :edn}]],
      :zd/docname 'mydoc.doc-4,
      :zd/subdoc? true,
      :title "Anonimous2"}])

  )

