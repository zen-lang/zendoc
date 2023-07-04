(ns zd.blocks.content
  (:require
   [hiccup.core :as h]
   [clojure.string :as str]
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
    (or (string? data) (number? data)) (pr-str data)
    (symbol? data) (link/symbol-link ztx data)
    (keyword? data) (str data)
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
  [:pre.mermaid d])

(defn mindmap-stack [stack lvl]
  (loop [[[slvl idx] & is :as st] stack]
    (if (nil? slvl)
      (list [lvl (inc (or (second (first stack)) -1))])
      (cond
        (< slvl lvl)  (conj st [lvl 0])
        (= slvl lvl)  (conj is [lvl (inc idx)])
        (> slvl lvl)  (recur is)))))

(defn mindmap-assoc [res stack content]
  (let [path (->> stack
                  reverse
                  (mapcat (fn [[_ idx]] [:children idx])))]
    (assoc-in res path {:name content :children []})))

(defn parse-mindmap [txt]
  (let [[root & lns] (->> (str/split-lines txt) (remove str/blank?))]
    (loop [[l & lns] lns
           stack     (list)
           res       {:name (str/trim root) :children []}]
      (if (nil? l)
        res
        (if (str/blank? l)
          (recur lns stack res)
          (let [len (count l)
                data (str/replace l #"^\s*\*" "")
                lvl (- len (count data))
                stack' (mindmap-stack stack lvl)]
            (recur lns stack' (mindmap-assoc res stack' (str/replace data #"^\s*" "")))))))))

(defmethod methods/rendercontent :mindmap
  [ztx ctx {ann :annotations data :data path :path :as block}]
  (let [id (str (gensym))]
    [:div
     [:svg.mindmap {:id id :width "912" :height "600" :margin "0px -30px"}]
     ;; TODO think about mounting mindmap and calling function elsewhere (in widget)
     [:script (str "mindmap('#" id "', " (cheshire.core/generate-string (parse-mindmap data)) ");")]]))
