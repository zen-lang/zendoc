(ns zd.view.core
  (:require
   [stylo.core :refer [c]]
   [zd.store :as store]
   [clojure.string :as str]
   [zd.view.doc]
   [zd.view.zentext]
   [zd.view.datalog]
   [zd.view.menu]
   [zd.view.multimedia]
   [zd.view.timeline]
   [zd.view.layout :as layout]))

(defn preview [ztx ctx doc]
  (zd.view.doc/view ztx ctx doc))

(defn page [ztx ctx doc]
  (layout/layout-with-menu ztx ctx doc (preview ztx ctx doc)))

(defn editor [ztx doc])

(defn errors [ztx doc])
