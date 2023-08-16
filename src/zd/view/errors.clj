(ns zd.view.errors
  (:require
   [stylo.core :refer [c]]
   [zd.store :as store]
   [zd.view.utils :as utils]
   [zd.methods :as methods]))

(defmethod methods/renderkey :zd/all-errors
  [ztx _ctx _block]
  [:div {:class (c)}
   (for [[docname errors] (->> (store/errors ztx)
                               (sort-by #(str (first %))))]
     [:div {:class (c [:py 1] [:mb 1])}
      [:div {:class (c [:pt 1] [:pb 0.5] :border-b [:mb 1])}
       [:b (utils/symbol-link ztx docname)]
       " "
       (str docname)]
      (for [e errors]
        [:div {:class (c :flex [:space-x 3] :borer-b :items-baseline)}
         [:div "⚬ " (str (:type e))]
         [:div (str (:path e))]
         [:div (:message e)]])])])
