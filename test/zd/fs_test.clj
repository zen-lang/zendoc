(ns zd.fs-test
  (:require
   [clojure.set :as set]
   [matcho.core :as matcho]
   [clojure.java.io :as io]
   [clojure.string :as str]
   [clojure.test :refer [deftest is testing]]
   [zd.fs :as fs]
   [zd.api]
   ;; [zd.memstore :as memstore]
   [zen.core :as zen]
   [zen-web.core :as web]
   [zd.test-utils :as t]))




(deftest test-fs

  (t/reset-project {'person ":title \"Person\"\n:zd/type zd.Class"
                    'org ":title \"Organizatin\""})

  (t/get-doc 'person)

  (t/hiccup-find (t/http-get "person") "title")

  (t/hiccup-match (t/http-get "person") "title"
                  [[:h1 {} "Person"]])

  (is (t/get-doc 'person))



  )

(defonce ztx (zen/new-context {}))

(comment
  (def ztx (zen/new-context {})))

(defn load! [ztx]
  (zen/read-ns ztx 'zd.test)
  (get-in (zen/start-call ztx 'zd.test/fs) [:zen/state :zd.fs]))

;; TODO add tests on document updates and deletion
;; check that memstore is reloaded correctly

(deftest document-tree-loaded
  (load! ztx)

  (is (nil? (agent-errors fs/queue)))

  (def docs (->> ['customers 'customers.flame 'people.john 'people.todd]
                 (map #(memstore/get-doc ztx %))))

  (testing "documents are loaded"
    (->> docs
         (every? (fn [{m :zd/meta :as doc}]
                   (is (map? m))
                   (is (not-empty m))
                   (is (seq (dissoc doc :zd/meta))))))))

(deftest macros-loaded
  (load! ztx)

  (is (nil? (agent-errors fs/queue)))

  (matcho/assert
   {:macro-notfound
    {:error
     {:message string?
      :type "macro-notfound"}}
    :yaml-example {:key "myvalue", :another-key "another-value"}
    :string-file "just a string\n"
    :not-found
    {:error {:message string?
             :type "macro-eval"}}
    :office-locations map?
    :zd/meta
    {:ann
     {:macro-notfound {:zd/macro list?}
      :yaml-example {:zd/macro list?}
      :string-file {:zd/macro list?}
      :not-found {:zd/macro list?}
      :office-locations {:zd/macro list?}}}}

   (memstore/get-doc ztx 'macros)))

(deftest referenced-loaded
  (load! ztx)

  (is (nil? (agent-errors fs/queue)))

  (testing "edn links loaded"
    (:zrefs @ztx)
    (matcho/match
     '{rdfs.class {customers #{[:meta :tags :#]}}
       people {people.john #{[:parent]}
               people.todd #{[:parent]}}
       people.john {customers #{[:best-customer] [:desc]}
                    customers.flame #{[:ceo] [:founder]}}
       customers {customers.flame #{[:parent]}
                  _schema #{[:parent]}
                  people #{[:parent]}}
       tags.data-platform {customers.flame #{[:tags :#]}}
       tags.telemed {customers.flame #{[:tags :#]}}}))

  (testing "zentext links and mentions loaded"
    (matcho/assert
     '{people.todd {customers #{[:desc]}}
       customers.flame {customers #{[:desc]}}
       people.john {customers #{[:product-champion] [:desc]}}}
     (:zrefs @ztx)))

  ;; TODO make links formats same?
  (testing "backlinks are collected"
    (matcho/assert {:zd/meta {:backlinks #{{:to 'customers.flame :path [:desc] :doc 'customers}}}}
                   (memstore/get-doc ztx 'customers.flame))

    (is (set/subset? #{{:to 'people.john :path [:desc] :doc 'customers}
                       {:to 'people.john :path [:product-champion] :doc 'customers}
                       {:to 'people.john :path [:ceo] :doc 'customers.flame}
                       {:to 'people.john :path [:founder] :doc 'customers.flame}}
                     (:backlinks (:zd/meta (memstore/get-doc ztx 'people.john)))))

    (is (set/subset? #{{:to 'people.todd :doc 'customers :path [:desc]}}
                     (:backlinks (:zd/meta (memstore/get-doc ztx 'people.todd)))))))

(deftest block-meta-added
  (load! ztx)

  #_(matcho/assert
   {:zd/meta
    {:ann {:rel {:zd/content-type :edn :badge {}}
           :tags {:zd/content-type :edn :badge {}}
           :icon {:zd/content-type :edn :none {}}
           :country {:zd/content-type :edn :badge {}}}}}
   (memstore/get-doc ztx 'customers.flame)))

(deftest subdocuments-loaded

  (load! ztx)

  (def doc (memstore/get-doc ztx 'customers))

  (matcho/assert
   {:zd/subdocs
    {:partners-list
     {:tags set?
      :countries set?}}}
   doc)

  (def subdoc-ann
    (get-in doc [:zd/subdocs :partners-list :zd/meta :ann]))

  ;; (is (contains? (:tags subdoc-ann) :badge))
  (is (contains? (:countries subdoc-ann) :badge)))
