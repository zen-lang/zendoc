(ns zd.parser-test
  (:require
   [matcho.core :as matcho]
   [zen.core :as zen]
   [clojure.java.io :as io]
   [zd.parser :as parser]
   [clojure.test :refer [deftest is]]))


(deftest test-parser

  (def to-read (slurp (io/resource "zd/parser/example.zd")))
  (def ztx (zen/new-context {}))

  (parser/parse-key-name ":key value")
  (parser/parse-key-name ":key {:a 1}")
  (parser/parse-key-name ":key edn/ {:a 1}")
  (parser/parse-key-name ": value")
  (parser/parse-key-name ":key | text")
  (parser/parse-key-name ":key |Long text")
  (parser/parse-key-name ":key type/ extra")
  (parser/parse-key-name ":key type/")
  (parser/parse-key-name ":key / extra")
  (parser/parse-key-name ":key /")
  (parser/parse-key-name ":key ?/")
  (parser/parse-key-name ":zd/key #{:a :b :c}")
  (parser/parse-annotation "^title | Long title")

  (parser/parse ztx 'mydoc to-read)

  (def result (parser/parse ztx 'mydoc to-read))

  (def doc
    {:zd/docname 'mydoc,
     :zd/view
     [[:string {:type :edn}]
      [:symbol {:type :edn}]
      [:symbols {:type :edn}]
      [:zd/require {:type :edn}]
      [:int {:type :edn}]
      [:long {:type :edn}]
      [:date {:type :edn}]
      [:map {:type :edn}]
      [:vector {:type :edn}]
      [:stringstyle {:type :str}]
      [:map-multiline {:type :edn, :multiline true}]
      [:multiline {:type :zentext, :multiline true}]
      [:with-block {:type :zentext, :multiline true}]
      [:with-title {:type :zentext, :multiline true, :title "Title"}]
      [:with-title-2 {:type :zentext, :multiline true, :title "| Here is a long title"}]
      [:multiple-annotations {:type :zentext, :multiline true, :title "Title", :as :none, :key {:a 1}}]
      [:query {:type :?, :multiline true}]],
     :string "Title",
     :symbol 'symbol,
     :symbols #{'a 'b}
     :zd/require #{:orgs/location :title :orgs/linkedin :orgs/tags :orgs/site},
     :int 1,
     :long 1.0,
     :date #inst "2001-01-01T00:00:00.000-00:00",
     :map {:a 1, :b 2},
     :vector [1 2 3 4],
     :stringstyle "Here is a long string",
     :map-multiline {:a 1, :b 2},
     :multiline "String 1\nString 2",
     :with-block "Text\n```\n:key-in-block \"value\"\n```\nText",
     :key-in-block nil?
     :with-title "Here is some",
     :with-title-2 "Here is a just a text",
     :multiple-annotations "line 1\nline 2",
     :query "e :zd/type zd.class\n> e"
     :zd/subdocs [{:zd/view [[:title {:type :edn}] [:local-ref {:type :edn}]],
                   :zd/docname 'mydoc.subdoc-1,
                   :zd/subdoc? true,
                   :zd/parent 'mydoc,
                   :title "S1",
                   :local-ref 'mydoc}
                  {:zd/view [[:title {:type :edn}]],
                   :zd/docname 'mydoc.subdoc-2
                   :zd/type 'zd/class,
                   :zd/subdoc? true,
                   :zd/parent 'mydoc
                   :title "S2"}
                  {:zd/view [[:title {:type :edn}]],
                   :zd/docname 'mydoc.doc-3,
                   :zd/subdoc? true,
                   :zd/parent 'mydoc,
                   :title "Anonimous1"}
                  {:zd/view [[:title {:type :edn}]],
                   :zd/docname 'mydoc.doc-4
                   :zd/type #{'zd/task 'zd/anything},
                   :zd/subdoc? true,
                   :zd/parent 'mydoc,
                   :title "Anonimous2"}]})

  (matcho/match result doc)

  )

