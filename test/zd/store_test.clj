(ns zd.store-test
  (:require
   [zd.store :as store]
   [zen.core :as zen]
   [matcho.core :as matcho]
   [clojure.test :refer [deftest is testing]]
   [zd.test-utils :as tu]))

(deftest store-unit-tests

  (is (=  (store/docname-to-path "a.b.c") "a/b/c.zd"))
  (is (=  (store/docname-to-path 'a.b.c) "a/b/c.zd"))
  (is (=  (store/path-to-docname "a/b/c.zd") 'a.b.c))
  (is (=  (store/child-docname 'a.b 'c) 'a.b.c))

  (def ztx (zen/new-context {:zd/dir ".tmp"}))
  (tu/rm-dir ".tmp")
  (tu/mk-dir ".tmp")

  (def doc-content ":zd/menu-order 1\n:title \"Index\"\n:attr ref\n&sub1\n:title \"Subdoc\"\n")
  (def other-content ":zd/menu-order 2\n:title \"Other\"\n:attr ref\n&sub1\n:title \"Subdoc\"\n")

  (spit ".tmp/index.zd" doc-content)
  (spit ".tmp/other.zd" other-content)

  (matcho/match (store/to-doc ztx 'mydoc doc-content {:parent 'index})
    {:title "Index"
     :zd/parent 'index
     :zd/subdocs [{:title "Subdoc" :zd/parent 'mydoc}]})

  (matcho/match (store/file-read ztx ".tmp" "index.zd")
    {:title "Index"
     :zd/subdocs [{:title "Subdoc"}]})

  (matcho/match (store/dir-read ztx ".tmp")
    [{:title "Index"
      :zd/subdocs [{:title "Subdoc" :zd/parent 'index}]}
     {:title "Other"
      ;; :zd/parent 'index
      :zd/subdocs [{:title "Subdoc" :zd/parent 'other}]}])

  (testing "encoding/decoding for datadog"
    (matcho/match
        (store/encode-data {:xt/id 'doc :zd/docname 'doc})
      {:xt/id "'doc", :zd/docname "'doc"})

    (matcho/match
        (store/decode-data   {:xt/id "'doc", :zd/docname "'doc"})
      {:xt/id 'doc :zd/docname 'doc})
    )

  (store/datalog-query
   ztx
   '{:where [[e :xt/id id]]
     :find [(pull e [*])]})

  (store/dir-load ztx ".tmp")

  (store/encode-query '{:where [[e :xt/id 'index] [e :title t]]
                        :find [e t]})


  (matcho/match
      (store/datalog-query
       ztx
       '{:where [[e :zd/parent p]]
         :find [e p]
         :order-by [[e :asc] [p :asc]]})
    '[[index index] [index.sub1 index] [other index] [other.sub1 other]])

  (matcho/match
      (store/get-backlinks ztx 'index)
    '{[:zd/parent] [index.sub1 other]})

  ;; (println :q (store/datalog-query ztx '{:where [[e :xt/id 'index] [e :title t]] :find [e t]}))

  (matcho/match
      (store/datalog-query ztx '{:where [[e :xt/id 'index] [e :title t]] :find [e t]})
    #{['index "Index"]})

  (matcho/match (store/doc-get ztx 'index)
    {:zd/docname 'index
     :title "Index"
     :zd/subdocs [{:zd/docname 'index.sub1
                   :title "Subdoc"}]})

  (matcho/match
      (store/datalog-query ztx '{:where [[e :xt/id 'index] [e :title t]] :find [e t]})
    #{['index "Index"]})

  (store/file-save ztx 'newone ":title \"newone\"\n&sub\n:title \"newonesub\"")

  (matcho/match (store/doc-get ztx 'newone)
    {:zd/docname 'newone
     :title "newone"
     :zd/parent 'index
     :zd/subdocs [{:zd/docname 'newone.sub}]})

  (matcho/match
      (store/get-backlinks ztx 'index)
    {[:zd/parent] '[index.sub1 other newone]})

  (matcho/match (store/doc-get ztx 'newone.sub)
    {:zd/docname 'newone.sub
     :zd/parent 'newone
     :zd/subdoc? true
     :title "newonesub"})

  (matcho/match (store/datalog-get ztx 'newone) {:title "newone"})
  (matcho/match (store/doc-get ztx 'newone) {:title "newone"})

  (matcho/match (store/datalog-get ztx 'newone.sub) {:title "newonesub"})
  (matcho/match (store/doc-get ztx 'newone.sub) {:title "newonesub"})

  (store/file-save ztx 'newone ":title \"newone-change\"\n")

  (matcho/match (store/datalog-get ztx 'newone) {:title "newone-change"})
  (matcho/match (store/doc-get ztx 'newone) {:title "newone-change"})

  (is (nil? (store/datalog-get ztx 'newone.sub)))

  (testing "errors"

    (store/file-save ztx 'newone ":title \"newone\"\n:broken broken")

    (matcho/match (store/datalog-get ztx 'newone)
      {:title "newone"
       :broken 'broken})

    (store/get-errors ztx 'newone)

    (matcho/match (store/doc-get ztx 'newone)
      {:title "newone"
       :broken 'broken
       :zd/errors [{:type :reference :path [:broken]}]})

    (store/errors ztx)

    (store/file-save ztx 'newone ":title \"newone\"\n:fixed index")

    (matcho/match (store/doc-get ztx 'newone)
      {:title "newone"
       :fixed 'index
       :zd/errors nil?})
    )

  (testing "backlinks"

    (store/file-save ztx 'target ":title \"target\"\n")
    (store/file-save ztx 'backref-1 ":title \"backref-1\"\n:ref target")
    (store/file-save ztx 'backref-2 ":title \"backref-2\"\n:ref target\n&sub\n:title \"sub\"\n:ref target")

    (:zd/backlinks @ztx)

    (matcho/match
        (store/doc-get ztx 'target)
      {:title "target"
       :zd/backlinks {[:ref] '[backref-1 backref-2 backref-2.sub]}})

    (store/file-delete ztx 'backref-2)

    (is (nil? (store/doc-get ztx 'backref-2)))
    (is (nil? (store/doc-get ztx 'backref-2.sub)))

    (matcho/match
        (store/doc-get ztx 'target)
      {:title "target"
       :zd/backlinks {[:ref] #(= '[backref-1] %)}})


    (testing "zentext refs"
      (store/file-save ztx 'zentext ":title \"zentext\"\n:desc /\nText #target")

      (store/doc-get ztx 'zentext)

      (matcho/match
          (store/doc-get ztx 'target)
        {:title "target"
         :zd/backlinks {[:desc] '[zentext]}})

      (store/file-delete ztx 'zentext)

      (matcho/match
          (store/doc-get ztx 'target)
        {:title "target"
         :zd/backlinks {'zentext nil?}})
      )
    )

  (testing "validation after delete"

    (store/file-save ztx 'a ":title \"a\"\n")
    (store/file-save ztx 'b ":title \"b\"\n:ref a")

    (matcho/match
        (store/doc-get ztx 'b)
      {:title "b"
       :zd/errors nil?})

    (matcho/match
        (store/doc-get ztx 'a)
      {:title "a"
       :zd/backlinks {[:ref] ['b]}})

    (store/file-delete ztx 'a)

    (matcho/match (store/doc-get ztx 'b)
      {:title "b"
       :zd/errors [{:type :reference }]}))

  (testing "navigation"
    (matcho/match (store/menu ztx)
      [{:zd/docname 'index}
       {:zd/docname 'other}])

    (store/file-save ztx 'to-menu ":title \"a\"\n:zd/menu-order 3")

    (matcho/match (store/menu ztx)
      [{:zd/docname 'index}
       {:zd/docname 'other}
       {:zd/docname 'to-menu}])

    (store/file-delete ztx 'to-menu)

    (matcho/match (store/menu ztx)
      [{:zd/docname 'index}
       {:zd/docname 'other}
       nil?]))

  (store/re-validate ztx)
  (is (store/errors ztx))

  (store/breadcrump ztx 'orgs.o1)

  (matcho/match
      (->
       (store/datalog-sugar-query ztx "e :xt/id id\n> e\n> e:title\n")
       (update :result #(sort-by first %)))
    '{:result
      [[b "b"]
       [backref-1 "backref-1"]
       [index "Index"]
       [index.sub1 "Subdoc"]
       [newone "newone"]
       [other "Other"]
       [other.sub1 "Subdoc"]
       [target "target"],]
     :query {:where [[e :xt/id id]] :find [(pull e [:xt/id :title])]},
     :columns [nil :title]})

  (is (contains? (store/props ztx) :fixed))

  (store/to-doc ztx 'mydoc ":title \"title\"\n^badge\n:key /\n value\n^ann 1\n:another some/\ntext")

  (testing "nested docs"

    (store/file-save ztx 'nested.one ":title \"one\"")
    (matcho/match
        (store/doc-get ztx 'nested.one)
      {:title "one"}))

)
