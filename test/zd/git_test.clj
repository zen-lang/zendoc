(ns zd.git-test
  (:require
   [zd.test-utils :as tu]
   [zd.git :as git]
   [matcho.core :as matcho]
   [clojure.test :refer [deftest is testing]]))

(deftest test-git
  (def dir ".tmp/git-test")
  (def ztx (tu/context dir))

  (git/init ztx)

  (tu/write-file ztx 'index ":title \"index\"")
  (git/commit ztx "initial commit")

  (matcho/match
      (git/history ztx 30)
    [{:date string?
      :commits
      [{:comment "initial commit",
        :user string?
        :time string?
        :files ["index.zd"]}]}])

  (tu/write-file ztx 'other ":title \"other\"")
  (tu/write-file ztx 'andonemore ":title \"andonemore\"")

  (matcho/match
      (git/changes ztx)
    [{:change :new, :file "andonemore.zd"}
     {:change :new, :file "other.zd"}])

  (git/commit ztx "second")

  (matcho/match
      (git/history ztx 30)
    [{:date string?
      :commits
      [{:comment "second"}
       {:comment "initial commit"}]}])

  (git/history ztx 30)

  (tu/write-file ztx 'other ":title \"change\"")
  (tu/write-file ztx 'new ":title \"new\"")

  (matcho/match
      (git/changes ztx)
    [{:change :modified, :file "other.zd"}
     {:change :new, :file "new.zd"}])


  (git/commit ztx "third")


  (matcho/match
      (git/history ztx 30)
    [{:date string?
      :commits
      [{:comment "third"}
       {:comment "second"}
       {:comment "initial commit"}]}])

  )
