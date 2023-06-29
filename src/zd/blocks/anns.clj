(ns zd.blocks.anns
  (:require
   [clojure.string :as str]
   [stylo.core :refer [c]]
   [zd.methods :as methods]
   [zd.components :as comp]))

(defmethod methods/renderann :title
  [ztx {doc :doc} {{title :title} :ann cnt :content k :key :as block}]
  [:div {:class (c [:py 4])}
   [:h2 {:class (c :flex :items-center [:m 0] [:py 4]) :id k}
    title]
   cnt])

(defmethod methods/renderann :table
  [ztx ctx {{headers :table} :ann cnt :content :as block}]
  (if (and (set? cnt)
           (every? vector? cnt)
           (map? (ffirst cnt))
           (seq headers))
    (comp/table ztx ctx headers (map first cnt))
    [:pre (pr-str cnt)]))

(defmethod methods/renderann :link-badge
  [ztx ctx {data :data k :key}]
  [:div {:class (c :border [:m 1]  :inline-flex :rounded [:p 0])}
   [:a {:href data
        :target "_blank"
        :class (c :inline-block
                  [:bg :gray-100]
                  [:hover [:bg :gray-200]]
                  [:px 2]
                  [:py 0.5]
                  [:text "#4B5BA0"]
                  :text-sm
                  {:font-weight "400"})}
    k]])

(defmethod methods/renderann :badge
  [ztx ctx {key :key cnt :content :as block}]
  [:div {:class (c :border [:my 1] [:mr 2] :inline-flex :items-baseline :rounded)}
   [:div {:class (c :inline-block [:px 1] [:bg :gray-100] [:py 0.5] :text-sm [:text :gray-700] {:font-weight "400"})}
    key]
   [:div {:class (c [:px 1] [:py 0.5] :inline-block :text-sm)}
    cnt]])

(defmethod methods/renderann :attribute
  [ztx ctx {k :key cnt :content :as block}]
  [:div {:title "attribute" :class (c [:py 0.5] :flex :items-center [:space-x 4])}
   [:div {:class (c  [:text :gray-600] {:font-weight "500"})} k]
   [:div cnt]])

(defmethod methods/renderann :none
  [ztx ctx block])

(defmethod methods/renderann :hide
  [ztx ctx block])

