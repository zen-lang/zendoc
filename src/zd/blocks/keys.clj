(ns zd.blocks.keys
  (:require
   [zd.methods :as methods]
   [zd.link :as link]
   [zd.memstore]
   [clojure.string :as str]
   [stylo.core :refer [c]]))


(defmethod methods/renderkey :errors-view
  [ztx ctx {d :data :as block}]
  [:div {:class (c)}
   (for [[docname errors] (->> (zd.memstore/get-all-errors ztx)
                               (sort-by #(str (first %))))]
     [:div {:class (c [:py 1] [:mb 1])}
      [:div {:class (c [:py 1] :border-b [:mb 1])} (str docname) " > "(link/symbol-link ztx docname)]
      (for [e errors]
        [:div {:class (c :flex [:space-x 3] :borer-b :items-baseline)}
         [:div "⚬ " (str (:type e))]
         [:div (str (:path e))]
         [:div (:message e)]])])])

;; TODO use link badge for linkedin prop?
(defmethod methods/renderkey :linkedin
  [ztx {{m :zd/meta} :doc} {data :data :as block}]
  (let [id (if (and data (not (str/blank? data)))
             data
             (last (str/split (str (:docname m)) #"\.")))]
    [:a {:class (str "badge " (name (c :border [:m 1]  :inline-flex :rounded
                                       [:py 1]
                                       :text-sm [:text :blue-500] [:px 2])))
         :id :linkedin
         :href
         (if (str/starts-with? (str (:docname m)) "organizations.")
           (str "https://www.linkedin.com/company/" id "/")
           (str "https://www.linkedin.com/in/" id "/"))}
     [:i.fa-brands.fa-linkedin]]))

(defmethod methods/renderkey :title
  [ztx {doc :doc} {title :data :as block}]
  [:h1 {:class (c :flex :items-center [:m 0] [:py 2] [:border-b :gray-400] [:mb 4]) :id "title"}
   (if-let [img (or (:avatar doc) (:logo doc))]
     [:img {:src img
            :class (c [:w 8] [:h 8] :inline-block [:mr 2] {:border-radius "100%"})}]
     (when-let [icon (or (:icon doc) (:zd/icon doc))]
       [:i {:class (str (str/join " " (map name icon))
                        " "
                        (name (c [:mr 2] [:text :gray-600])))}]))
   title])
