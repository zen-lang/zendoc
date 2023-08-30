(ns zd.store-test
  (:require
   [zd.store :as store]
   [matcho.core :as matcho]
   [clojure.set]
   [clojure.test :refer [deftest is testing]]
   [zd.test-utils :as tu]))

(deftest store-unit-tests

  (is (=  (store/docname-to-path "a.b.c") "a/b/c.zd"))
  (is (=  (store/docname-to-path 'a.b.c) "a/b/c.zd"))
  (is (=  (store/path-to-docname "a/b/c.zd") 'a.b.c))
  (is (=  (store/child-docname 'a.b 'c) 'a.b.c))

  (def ztx (tu/context ".tmp/integration"))

  (tu/write-file ztx 'index ":zd/menu-order 1\n:title \"Index\"\n&sub1\n:title \"Subdoc\"\n")
  (tu/write-file ztx 'other ":zd/menu-order 2\n:title \"Other\"\n&sub1\n:title \"Subdoc\"\n")

  (matcho/match (store/to-doc ztx 'mydoc ":zd/menu-order 1\n:title \"Index\"\n&sub1\n:title \"Subdoc\"\n" {:zd/parent 'index})
    {:title "Index"
     :zd/parent 'index
     :zd/subdocs [{:title "Subdoc" :zd/parent 'mydoc}]})

  (matcho/match (store/file-read ztx (:zd/dir @ztx) "index.zd")
    {:title "Index"
     :zd/subdocs [{:title "Subdoc"}]})

  (matcho/match (store/dir-read ztx)
    [{:title "Index"
      :zd/subdocs [{:title "Subdoc" :zd/parent 'index}]}
     {:title "Other"
      :zd/parent 'index
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

  (store/dir-load ztx)


  (is (nil? (store/errors ztx)))

  (matcho/match
      (store/datalog-query
       ztx
       '{:where [[e :zd/parent p]]
         :find [e p]
         :order-by [[e :asc] [p :asc]]})
    '[[errors index] [index index] [index.sub1 index] [other index] [other.sub1 other]])

  (matcho/match
      (store/get-backlinks ztx 'index)
    '{[:zd/parent] [errors  other]})


  ;; (println :q (store/datalog-query ztx '{:where [[e :xt/id 'index] [e :title t]] :find [e t]}))

  (matcho/match
      (store/datalog-query ztx '{:where [[e :xt/id 'index] [e :title t]] :find [e t]})
    #{['index "Index"]})

  (tu/doc? ztx 'index
    {:zd/docname 'index
     :title "Index"
     :zd/subdocs [{:zd/docname 'index.sub1
                   :title "Subdoc"}]})

  (matcho/match
      (store/datalog-query ztx '{:where [[e :xt/id 'index] [e :title t]] :find [e t]})
    #{['index "Index"]})

  (store/file-save ztx 'newone ":title \"newone\"\n&sub\n:title \"newonesub\"")

  (tu/doc? ztx 'newone
    {:zd/docname 'newone
     :title "newone"
     :zd/parent 'index
     :zd/subdocs [{:zd/docname 'newone.sub}]})

  (matcho/match
      (store/get-backlinks ztx 'index)
    {[:zd/parent] '[errors newone other]})


  (tu/doc?  ztx 'newone.sub
    {:zd/docname 'newone.sub
     :zd/parent 'newone
     :zd/subdoc? true
     :title "newonesub"})

  (matcho/match (store/datalog-get ztx 'newone) {:title "newone"})
  (tu/doc?  ztx 'newone {:title "newone"})

  (matcho/match (store/datalog-get ztx 'newone.sub) {:title "newonesub"})
  (tu/doc?  ztx 'newone.sub {:title "newonesub"})

  (store/file-save ztx 'newone ":title \"newone-change\"\n")

  (matcho/match (store/datalog-get ztx 'newone) {:title "newone-change"})
  (tu/doc? ztx 'newone  {:title "newone-change"})

  (is (nil? (store/datalog-get ztx 'newone.sub)))

  (testing "errors"

    (store/file-save ztx 'newone ":title \"newone\"\n:broken broken")

    (matcho/match (store/datalog-get ztx 'newone)
      {:title "newone"
       :broken 'broken})

    (store/get-errors ztx 'newone)

    (tu/doc?  ztx 'newone
      {:title "newone"
       :broken 'broken
       :zd/errors [{:type :reference :path [:broken]}]})

    (store/errors ztx)

    (store/file-save ztx 'newone ":title \"newone\"\n:fixed index")

    (tu/doc? ztx 'newone
      {:title "newone"
       :fixed 'index
       :zd/errors nil?})
    )

  (testing "backlinks"

    (store/file-save ztx 'target ":title \"target\"\n")
    (store/file-save ztx 'backref-1 ":title \"backref-1\"\n:ref target")
    (store/file-save ztx 'backref-2 ":title \"backref-2\"\n:ref target\n&sub\n:title \"sub\"\n:ref target")

    (:zd/backlinks @ztx)

    (tu/doc? ztx 'target {:title "target" :zd/backlinks {[:ref] '[backref-1 backref-2 backref-2.sub]}})

    (store/file-delete ztx 'backref-2)

    (is (nil? (store/doc-get ztx 'backref-2)))
    (is (nil? (store/doc-get ztx 'backref-2.sub)))

    (tu/doc? ztx 'target {:title "target" :zd/backlinks {[:ref] #(= '[backref-1] %)}})


    (testing "zentext refs"
      (store/file-save ztx 'zentext ":title \"zentext\"\n:desc /\nText #target")
      (store/doc-get ztx 'zentext)
      (tu/doc? ztx 'target {:title "target" :zd/backlinks {[:desc] '[zentext]}})
      (store/file-delete ztx 'zentext)
      (tu/doc? ztx 'target {:title "target" :zd/backlinks {'zentext nil?}})
      )
    )

  (testing "validation after delete"

    (store/file-save ztx 'a ":title \"a\"\n")
    (store/file-save ztx 'b ":title \"b\"\n:ref a")

    (tu/doc? ztx 'b {:title "b" :zd/errors nil?})
    (tu/doc? ztx 'a {:title "a" :zd/backlinks {[:ref] ['b]}})

    (store/file-delete ztx 'a)

    (tu/doc? ztx 'b {:title "b" :zd/errors [{:type :reference }]}))

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
       (store/datalog-sugar-query ztx "e :xt/id id\n> e\n> e:title\n < asc e")
       (update :result #(sort-by first %)))
    '{:result
      [[b "b"]
       [backref-1 "backref-1"]
       [errors "Errors"]
       [index "Index"]
       [index.sub1 "Subdoc"]
       [newone "newone"]
       [other "Other"]
       [other.sub1 "Subdoc"]
       [target "target"]]})


  (store/to-doc ztx 'mydoc ":title \"title\"\n^badge\n:key /\n value\n^ann 1\n:another some/\ntext")

  (testing "nested docs"

    (store/file-save ztx 'nested.one ":title \"one\"")
    (tu/doc? ztx 'nested.one {:title "one"}))

  (testing "props"
    (is (clojure.set/subset?  #{":zd/subdoc?" ":broken"} (into #{} (mapv :name (store/props ztx))))))

  (testing "rename"
    (store/file-save ztx 'torename ":title \"torename\"\n:ref other")

    (is (store/file-content ztx 'torename))

    (tu/doc? ztx 'torename {:title "torename"})

    (is (contains? (store/backlinked ztx 'index) 'torename))

    (store/file-save ztx 'torename ":zd/docname renamed\n:title \"torename\"\n:ref other")

    (is (tu/file-exists ztx "renamed.zd"))
    (is (not (tu/file-exists ztx "torename.zd")))

    (is (nil? (store/doc-get ztx 'torename)))

    (is (nil? (store/file-content ztx 'torename)))

    (is (store/file-content ztx 'renamed))

    (tu/doc? ztx 'renamed {:title "torename"})

    (is (not (contains? (store/backlinked ztx 'index) 'torename)))
    (is (contains? (store/backlinked ztx 'index) 'renamed))

    )

  (testing "zd root is preloaded"
    (tu/doc? ztx 'zd {:title "Zendoc"}))

  (testing "errors page is preloaded"
    (is (seq (store/errors ztx)))
    (tu/doc? ztx 'errors {:zd/all-errors true})

    )

  )

(deftest test-link-errors
  (def ztx (tu/context ".tmp/link-errors"))

  (tu/write-file ztx {:zd/docname 'index :other 'other})
  (tu/write-file ztx {:zd/docname 'other :index 'index :text "text"})
  (tu/write-file ztx {:zd/docname 'other.child :index 'index})
  (tu/write-file ztx {:zd/docname 'other.child2 :index 'index})

  (store/dir-load ztx)

  (tu/doc?
   ztx 'index
   {:zd/docname 'index,
    :other 'other
    :zd/errors nil?
    :zd/backlinks {[:index] '[other other.child other.child2]}})

  (tu/doc?
   ztx 'other.child
   {:zd/docname 'other.child
    :zd/errors nil?})

  (is (empty? (store/errors ztx)))

  (store/file-save ztx 'other ":zd/title \"Other\"")

  (is (empty? (store/errors ztx))))

(deftest test-errors-fixed
  (def ztx (tu/context ".tmp/errors-fixed"))

  (tu/write-file ztx {:zd/docname 'index :link 'unexisting})

  (:zd/classes @ztx)

  (store/dir-load ztx)

  (tu/doc?
   ztx 'index
   {:zd/docname 'index,
    :link 'unexisting
    :zd/errors [{:type :reference,
                 :message "`unexisting` not found",
                 :path [:link]}]})

  ;; (assert false "!")

  (matcho/match
      (store/errors ztx)
    '{index [{:type :reference, :message "`unexisting` not found", :path [:link]}]})

  (store/backlinked ztx 'unexisting)

  (store/file-save ztx 'unexisting ":zd/title \"Fixed\"")

  (is (empty? (store/errors ztx)))
  )

(deftest test-errors-appers
  (def ztx (tu/context ".tmp/errors-appers"))

  (tu/write-file ztx {:zd/docname 'index :link 'existing})
  (tu/write-file ztx {:zd/docname 'existing})

  (store/dir-load ztx)
  (store/backlinked ztx 'existing)

  (is (empty? (store/errors ztx)))

  (store/file-delete ztx 'existing)

  (matcho/match (store/errors ztx)
    {'index [{:type :reference :path [:link]}]})
  )



(deftest test-backlinks
  (def ztx (tu/context ".tmp/errors-appers"))

  (tu/write-file ztx {:zd/docname 'index})

  (tu/write-file ztx 'org "
:zd/type zd.class
:zd/summary  [:org/name :org/address]
:zd/require #{:org/name}
&name zd.prop
&address zd.prop
:zd/summary true
")

  (tu/write-file ztx 'org.o1 "
:zd/type org
:org/name \"o1\"
:org/address \"a1\"
:extra 5
")

  (tu/write-file ztx 'org.o2 "
:zd/type org
:org/name \"o2\"
:org/address \"a2\"
:extra 5
")

  (store/dir-load ztx)

  (store/search ztx "o2")
  (store/search ztx "")
  (store/search ztx nil)

  (tu/doc? ztx 'org
           {:zd/backlinks
            '{[:zd/parent org] [org.o1 org.o2,] [:zd/type org] [org.o1 org.o2]}})

  )

(deftest test-rename
  (def ztx (tu/context ".tmp/test-rename"))

  (tu/write-file ztx {:zd/docname 'org})
  (tu/write-file ztx {:zd/docname 'org.o1})
  (tu/write-file ztx {:zd/docname 'org.o2})
  (tu/write-file ztx {:zd/docname 'org.o2.item1})

  (store/dir-load ztx)

  (is (tu/file-exists ztx "org.zd"))
  (is (tu/file-exists ztx "org/o1.zd"))
  (is (tu/file-exists ztx "org/o2.zd"))
  (is (tu/file-exists ztx "org/o2/item1.zd"))

  (matcho/match
      (store/children ztx 'org)
    #{'org.o2 'org.o1})

  (store/file-save ztx 'org ":zd/docname organization")

  (is (tu/file-exists ztx "organization.zd"))
  (is (tu/file-exists ztx "organization/o1.zd"))
  (is (tu/file-exists ztx "organization/o2.zd"))
  (is (tu/file-exists ztx "organization/o2/item1.zd"))


  (is (not (tu/file-exists ztx "org.zd")))
  (is (not (tu/file-exists ztx "org/o1.zd")))
  (is (not (tu/file-exists ztx "org/o2.zd")))
  (is (not (tu/file-exists ztx "org/o2/item1.zd")))

  (store/file-delete ztx 'organization)

  (is (not (tu/file-exists ztx "organization.zd")))
  (is (not (tu/file-exists ztx "organization/o1.zd")))
  (is (not (tu/file-exists ztx "organization/o2.zd")))
  (is (not (tu/file-exists ztx "organization/o2/item1.zd")))

  )

(deftest test-inference
  (def ztx (tu/context ".tmp/test-inference"))

  (tu/write-file ztx {:zd/docname 'index})
  (tu/write-file ztx {:zd/docname 'org  :zd/child-type 'org :zd/require #{:a :b}})
  (tu/write-file ztx {:zd/docname 'org.o1})

  (store/dir-load ztx)

  (matcho/match
   (store/doc-get ztx 'org.o1)
   #:zd{:errors
        [{:type :required, :message ":b is required", :path [:b]}
         {:type :required, :message ":a is required", :path [:a]}]})

  (store/errors ztx)

  )
