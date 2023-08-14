(ns zd.view.datalog
  (:require [zd.view.utils :as utils]
            [zd.zentext :as zentext]
            [clojure.string :as str]
            [clojure.pprint]
            [zd.store :as store]
            [stylo.core :refer [c]]
            [zd.methods :as methods]))




(defn render-table-value [ztx v block]
  (cond
    (symbol? v) (utils/symbol-link ztx v)
    (string? v) (zentext/parse-block ztx v block)
    (number? v) v
    (nil? v) "~"
    (set? v) [:div {:class (c :flex :items-center [:space-x 2])}
              (->> v (map (fn [x] [:div {:key (str x)} (render-table-value ztx x block)])))]
    (keyword? v) (str v)
    (inst? v) (str/replace (str v) #"00:00:00 " "")
    :else (with-out-str (clojure.pprint/pprint v))))

(defmethod methods/rendercontent :?
  [ztx _ctx {data :data :as block}]
  (let [{res :result cols :columns q :query} (store/datalog-sugar-query ztx data)]
    [:div
     [:table
      (into [:thead]
            (->> cols
                 (map (fn [col]
                        [:th {:class (c [:py 1] [:px 2] [:bg :gray-100] :border {:font-weight "500"})}
                         (cond (keyword? col) (name col)
                               (nil? col) "~"
                               :else (str col))]))))
      (into [:tbody]
            (for [vs res]
              [:tr {:key (hash vs)}
               (for [v vs]
                 [:td {:key v :class (c :border [:px 2] [:py 1] {:vertical-align "top"})}
                  (render-table-value ztx v block)])]))]
     (utils/pprint "query" (dissoc q :columns :index :args))]))
