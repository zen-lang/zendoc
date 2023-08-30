(ns zd.view.search
  (:require
   ;; [zd.runner :as runner]
   [stylo.core :refer [c]]
   [zd.methods :as methods]
   [zd.view.utils :as utils :refer [btn-c]]
   [clojure.string :as str]))

(defn search-view [ztx {res :results q :query}]
  [:div
   [:form {:action "/_search"}
    [:input#search {:class (c :block :border [:px 4] [:py 2] :rounded {:width "100%"})
                    :value (or q "") :name "query"}]]
   [:div#results {:class (c :divide-y)}
    (->> res
         (map (fn [{id :zd/docname :as doc}]
                [:div {:class (c [:py 1] :flex :items-baseline [:space-x 4])}
                 [:div {:class (c :flex-1)} (utils/symbol-link ztx id)]
                 [:div {:class (c :text-sm [:text :gray-600])} (when-let [desc (:desc doc)]
                                                                 (subs desc 0 (min (count desc) 100)))]])))]
   [:script "
    var search = document.getElementById('search');
    search.focus();
    var end = search.value.length;
    search.setSelectionRange(end, end);"]])
