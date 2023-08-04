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
   [clojure.walk]
   [edamame.core]
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



(defn parse-query [q]
  (let [xs (->> (str/split q #"\n")
                (remove (fn [s] (or (str/blank? s) (str/starts-with? s "\\")))))
        columns   (->> xs
                       (filterv #(re-matches #"^\s?>.*" %))
                       (mapv #(subs % 1))
                       (mapv str/trim)
                       (filterv #(not (str/blank? %)))
                       (mapv (fn [x]
                               (if (str/starts-with? x "(")
                                 ['expr (edamame.core/parse-string x {:regex true})]
                                 (let [[e k] (str/split x #":" 2)]
                                   [(symbol e) (cond
                                                 (= k "*") (symbol k)
                                                 (nil? k) :meta/docname
                                                 :else (keyword k))])))))
        index (atom {})
        find-items (->> (group-by first columns)
                        (reduce (fn [acc [k xs]]
                                  (if (= 'expr k)
                                    (->> (mapv second xs)
                                         (reduce (fn [acc e]
                                                   (swap! index assoc e (count acc))
                                                   (conj acc e))
                                                 acc))
                                    (let [cs (->> (mapv second xs) (dedupe) (into []))]
                                      (swap! index assoc k (count acc))
                                      (if (filter (fn [x] (contains? #{'x :?} x)) cs)
                                        (conj acc (list 'pull k ['*]))
                                        (if (seq cs)
                                          (conj acc (list 'pull k cs))
                                          (conj acc k))))))
                                []))
        where-items
        (->> xs
             (filterv (every-pred #(not (str/ends-with? % " :asc"))
                                  #(not (str/ends-with? % " :desc"))
                                  #(not (re-matches #"^\s?>.*" %))))
             (mapv (fn [x] (let [res (edamame.core/parse-string (str/replace (str "[" x "]") #"#"  ":symbol/")
                                                                {:regex true})]
                             (cond
                               (list? (get res 1))
                               (vector res)

                               :else
                               res)
                             ))))
        where (->> where-items
                   (mapv (fn [x]
                           (clojure.walk/postwalk
                            (fn [y]
                              (if (and (keyword? y) (= "symbol" (namespace y)))
                                (str "'" (name y))
                                y)) x))))

        order-items
        (->> xs
             (filterv (every-pred #(or (str/ends-with? % " :asc")
                                       (str/ends-with? % " :desc"))
                                  #(not (re-matches #"^\s?>.*" %))))
             (mapv (fn [x] (edamame.core/parse-string (str/replace (str "[" x "]") #"#"  ":symbol/") {:regex true}))))

        order
        (->> order-items
             (mapv (fn [x]
                     (clojure.walk/postwalk
                       (fn [y]
                         (if (and (keyword? y) (= "symbol" (namespace y)))
                           (str "'" (name y))
                           y)) x))))]
    (into {:where where
           :order order
           :find find-items
           :columns columns
           :index @index} )))



(def q
  "
e :parent #organizations
e :rel #rel.partner
p :organization e
p :role #roles.cto
> d
> e:xt/id
> e:rel
> (count e)
> (mean e)
")

(def q2
  "
e :parent #customers
e :category cat
(clojure.string/starts-with? cat \"s\")
e :customer-since since
e :asc

> e:name
> (count e)
"
  )

(def q3
  "
e :parent #customers
e asc
> e
")

(parse-query q3)

(defn render-table-value [ztx v block]
  (cond
    (symbol? v) (link/symbol-link ztx v)
    (string? v) (zentext/parse-block ztx v block)
    (number? v) v
    (nil? v) "~"
    (set? v) [:div {:class (c :flex :items-center [:space-x 2])}
              (->> v (map (fn [x] [:div {:key (str x)} (render-table-value ztx x block)])))]
    (keyword? v) (str v)
    :else (with-out-str (clojure.pprint/pprint v))))

(defmethod methods/rendercontent :?
  [ztx ctx {{headers :table-of} :ann data :data :as block}]
  (let [q (parse-query data)
        idx (:index q)
        res (d/query ztx (dissoc q :columns :index))
        res (->> res
                 (mapv (fn [x]
                         (->> (:columns q)
                              (mapv (fn [[e c]]
                                      (cond
                                        (list? c) (get-in x [(get idx c)])
                                        (= c '*)  (get-in x [(get idx e)])
                                        (= c :?)  (keys (get-in x [(get idx e)]))
                                        :else     (get-in x [(get idx e) c]))))))))
        cols (->> (:columns q) (mapv second))]
    [:div
     [:details {:class (c :text-xs [:mb 2])}
      [:summary {:class (c [:text :gray-500]) }"query"]
      [:pre {:class (c :border [:p 2] [:bg :gray-100])}
       (with-out-str (clojure.pprint/pprint (dissoc q :columns :index :args)))]]
     [:table
      (into [:thead]
            (->> cols (map (fn [col] [:th {:class (c [:py 1] [:bg :gray-100] :border {:font-weight "500"})} (str col)]))))
      (into [:tbody]
            (for [vs res]
              [:tr {:key (hash vs)}
               (for [v vs]
                 [:td {:key v :class (c :border [:px 2] [:py 1] {:vertical-align "top"})}
                  (render-table-value ztx v block)])]))]]))

(defmethod methods/rendercontent :mm
  [ztx ctx {d :data :as block}]
  (let [id (str (gensym))
        scr
        (format
         "var id = '%s';
          var mm = document.getElementById(id).innerText;
          mermaid.render('unexisting-id', mm).then((res)=>{
           document.getElementById(id).innerHTML = res.svg;
          });
          "
         id)]
    [:div
     [:pre {:id id} d]
     [:script {:type "module"} scr]]))

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
