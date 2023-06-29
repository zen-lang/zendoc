(ns zd.blocks.content
  (:require
   [zd.components :as comp]
   [zd.datalog :as d]
   [zd.zentext :as zentext]
   [zd.link :as link]
   [clojure.pprint :as pprint]
   [stylo.core :refer [c]]
   [zd.methods :as methods]
   [zd.utils :as utils]))

(defmethod methods/rendercontent :edn
  [ztx ctx {:keys [data] :as block}]
  (cond
    (or (string? data) (number? data)) (zentext/parse-block ztx (str data) block)
    (symbol? data) (link/symbol-link ztx data)
    (keyword? data) (zentext/parse-block ztx (str data) block)
    (or (set? data) (vector? data))
    (->> data
         (mapv (fn [x] (methods/rendercontent ztx ctx (assoc block :data x))))
         (into [:div {:class (c :flex [:space-x 1.5] {:flex-wrap "wrap"})}
                [:div {:class (c [:text :gray-500] :text-sm)} "#"]]))
    :else
    [:div {:style {:background "white" :word-wrap "break-word"}}
     (if (string? data)
       data
       (with-out-str (pprint/pprint data)))
     ;; TODO fix clipboard copy
     #_[:pre {:class (c :text-sm) :style {:white-space "pre-wrap"}}
      [:i.fas.fa-clipboard-list.copy-button
         {:title "Click to Copy"
          :style {:position  "relative"
                  :float     "right"
                  :top       "5px"
                  :right     "20px"}}]]]))

(defmethod methods/rendercontent :zentext
  [ztx ctx {:keys [data] :as block}]
  [:div (zentext/parse-block ztx data block)])

(defmethod methods/rendercontent :datalog
  [ztx ctx {{headers :table-of} :ann data :data :as block}]
  (if-let [params (:in data)]
    ;; TODO impl preprocess to remove :in from queries
    (apply d/query ztx data params)
    (d/query ztx data)))

(defmethod methods/rendercontent :mm
  [ztx ctx {d :data :as block}]
  [:div.mermaid d])


