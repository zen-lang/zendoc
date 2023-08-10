(ns zd.blocks.zd
  (:require [zd.link :as link]
            [zd.zentext :as zentext]
            [zd.datalog :as d]
            [zd.memstore :as memstore]
            [zd.meta :as meta]
            [clojure.string :as str]
            [stylo.core :refer [c]]
            [zd.methods :as methods]))

(defn indent-item [pth]
  (let [lvl (count pth)
        scale 1]
    [:div {:class (c :flex [:py 1] [:px 1] :items-center {:position "relative"})
           :style (str "padding-left:" (* scale (- lvl 1)) "rem")}
     (map-indexed (fn [i el]
                    [:div
                     {:style (when-not (= lvl 1)
                               "border-left: 1px solid #ccc;")
                      :class (c [:w 2] {:top "-0.3rem"
                                        :position "absolute"
                                        :bottom 0})}])
                  pth)
     (when-not (= lvl 1)
       [:div {:style (str "left:" (* scale lvl) "rem")
              :class (c {:border-top "1px solid #ccc"
                         :width "0.5rem"
                         :top "13px"})}])]))

(defmethod methods/renderkey :zd/index
  [ztx ctx block]
  (let [docs (->> '{:find [?docname]
                    :where [[?e :xt/id ?docname]
                            [?e :title ?title]]
                    :order-by [[?docname :asc]]}
                  (d/query ztx)
                  (mapv (fn [v] {:s (first v) :ps (str/split (str (first v)) #"\.")}))
                  (partition-by #(= 1 (count (:ps %))))
                  (partition 2)
                  (mapv (fn [[h t]] (if (> (count t) 20) h (concat h t)))))
        total (reduce + (map count docs))
        group-idx (loop [[[idx item] & oth] (map-indexed vector (map count docs))
                         left (int (/ total 2))]
                    (if (<= left 0)
                      idx
                      (recur oth (- left item))))
        [l r] (split-at group-idx docs)]
    [:div {:class (c :flex :flex-row)}
     [:div {:class (c :flex [:w "50%"])}
      [:div {:class (c [:py 2] [:w "50%"] :text-sm :flex-col :flex)}
       (doall
        (for [{:keys [s ps]} (apply concat l)]
          [:div (conj (indent-item ps)
                      (link/symbol-link ztx s))]))]]
     [:div {:class (c :flex [:w "50%"])}
      [:div {:class (c [:py 2] [:w "50%"] :text-sm :flex-col :flex)}
       (doall
        (for [{:keys [s ps]} (apply concat r)]
          [:div (conj (indent-item ps)
                      (link/symbol-link ztx s))]))]]]))

(defmethod methods/renderkey :zd/docname
  [ztx ctx {data :data :as block}]
  [:div {:class (str "badge " (name (c :border [:my 1] [:mr 2]  :inline-flex :rounded [:p 0])))}
   [:div {:class (c :inline-block [:px 2] [:bg :gray-100] [:py 0.5] :text-sm [:text :gray-700]
                    {:font-weight "400" :padding-top ".3rem"})}
    [:i.fas.fa-file]]
   [:div {:class (c [:px 2] [:py 0.5] :inline-block :text-sm)}
    data]])

(defmethod methods/renderkey :zd/rename
  [ztx ctx {data :data :as block}]
  [:div {:class (str "badge " (name (c :border [:my 1] [:mr 2]  :inline-flex :rounded [:p 0])))}
   [:div {:class (c :inline-block [:px 2] [:bg :gray-100] [:py 0.5] :text-sm [:text :gray-700]
                    {:font-weight "400" :padding-top ".3rem"})}
    [:i.fas.fa-arrow-right]]
   [:div {:class (c [:px 2] [:py 0.5] :inline-block :text-sm)}
    data]])

(defmethod methods/renderkey :zd/backlinks
  [ztx {{{dn :docname} :zd/meta} :doc {qs :query-string} :request r :root :as ctx} {:keys [data] :as block}]
  (let [summary-keys (meta/get-group ztx :zd/summary)
        ;; TODO move this processing to memstore
        links
        (->> data
             (mapcat (fn [[from paths]] (for [p paths] {:to dn :path p :doc from})))
             (map (fn [{d :doc p :path t :to}]
                    {:to t
                     :doc d
                     :parent (when (str/includes? (str d) ".") (str/join "." (butlast (str/split (str d) #"\."))))
                     :path   (->> (map (fn [f] (if (keyword? f) (name f) (str f))) p) (str/join ".") (str ":"))}))
             (sort-by (juxt :parent :path :doc))
             (group-by :parent)
             (map (fn [[p links]] [p (sort-by (fn [{d :doc}] (:title (memstore/get-doc ztx d))) links)])))]
    (for [[parent links] links]
      (let [*parent (or parent r)]
        [:div {:class (c [:py 4] #_[:text :gray-600])}
         [:div {:class (c :flex :flex-row :items-center :border-b :justify-between [:py 1])}
          [:div
           [:span {:class (c :text-sm [:mr 1])}
            [:i.fa-solid.fa-arrow-up]]
           [:a {:id (str "backlinks-" *parent)
                :class (c :text-lg {:font-weight "600"})}
            (str *parent ".*")]
           [:span {:class (c [:pl 2] :text-sm [:text :gray-500])}
            (str/join ", " (set (map :path links)))]]]
         ;; TODO think if path is needed in each link
         (for [{docname :doc} (distinct (map #(dissoc % :path) links))]
           (let [{{anns :ann} :zd/meta :as doc} (memstore/get-doc ztx (symbol docname))]
             [:div {:class (c [:py 1] :flex  :border-b)}
              [:div {:class (c :flex :flex-1)}
               (link/symbol-link ztx docname {:force-icon true :icon-class (c {:min-width "1em"})})
               [:div {:class (c :flex :self-center)}
                (when (str/includes? (str docname) "_template")
                  [:span {:class (c :text-xs [:text :orange-500] [:pl 2])}
                   "_template"])
                (when (str/includes? (str docname) "_schema")
                  [:span {:class (c :text-xs [:text :orange-500] [:pl 2])}
                   "_schema"])]]
              [:div {:class (c :flex :flex-wrap :overflow-x-hidden)}
               (doall
                (for [[k v] (select-keys doc summary-keys)]
                  (when (and (some? v) (= (get-in anns [k :zd/content-type]) :edn))
                    [:div {:class (c :inline-flex :text-sm :items-center [:mr 2.2])}
                     [:div {:class (c [:mr 0.5])}
                      (str (name k) ":")]
                     (cond
                       ;; render links collection
                       (or (set? v) (vector? v))
                       (into [:div {:class (c :flex [:space-x 1] :items-center)}]
                             (interpose
                              [:span {:class (c [:m 0] [:p 0])} ","]
                              (mapv
                               (fn [s]
                                 (if (symbol? s)
                                   [:a {:href (str "/" s)
                                        :class (c :inline-flex
                                                  :items-center
                                                  [:hover [:text :blue-600] :underline]
                                                  :whitespace-no-wrap
                                                  {:text-decoration-thickness "0.5px"})}
                                    [:span (if-let [res (memstore/get-doc ztx s)]
                                             (:title res)
                                             (str s))]]
                                   [:span (pr-str s)]))
                               v)))
                       (symbol? v)
                       (link/symbol-link ztx v)
                       :else [:span (pr-str v)])])))]]))]))))

;; TODO: obsolete
(defmethod methods/renderkey :zd/errors
  [ztx ctx {errors :data k :key :as block}]
  [:div {:class (c [:text :red-700]  [:my 2] :rounded :text-sm [:border :red-300])}
   [:ul {:class (c :font-bold [:mb 1] [:py 1] [:px 3] [:ml 0] [:text :red-600] [:bg :red-100] [:border-b :red-300])} "Document errors"]
   (for [err (sort-by :type errors)]
     [:li {:class (c [:py 0.5] :flex [:space-x 3] [:text :red-600] [:px 3])}
      [:span (pr-str (:path err))]
      [:span {:class (c [:ml 4] {:text-align "right"})} (:message err)]])])


