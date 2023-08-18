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
   [zd.view.timeline :as timeline]
   [zd.view.editor]
   [zd.view.layout :as layout]
   [zd.view.errors]
   [zd.view.utils]
   [zd.view.search :as search]
   [hiccup.core]))

(defn preview [ztx ctx doc]
  (zd.view.doc/preview ztx ctx doc))

(defn view [ztx ctx doc]
  (zd.view.doc/view ztx ctx doc))

(defn page [ztx ctx doc]
  (layout/layout-with-menu ztx ctx doc (view ztx ctx doc)))

(defn editor [ztx ctx doc content]
  (layout/layout ztx ctx (zd.view.editor/editor ztx ctx doc  content)))

(defn timeline [ztx ctx data]
  (layout/layout-with-menu ztx ctx {:zd/docname 'git} (timeline/view ztx data)))

(defn search [ztx ctx params]
  (layout/layout-with-menu ztx ctx {:zd/docname 'search} (search/search-view ztx params)))
