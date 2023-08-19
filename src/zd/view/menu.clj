(ns zd.view.menu
  (:require [zd.store :as store]
            [clojure.string :as str]
            [zd.view.utils :as utils]
            [stylo.core :refer [c]]))

(defn menu [ztx {{{search-text :search} :query-params :as req} :request r :root :as ctx} doc]
  [:div#left-nav {:class (c :border-r
                            [:pt 8]
                            [:bg "#fbfbfb"]
                            [:h "100vh"]
                            :overflow-y-auto
                            [:px 8]
                            [:w 80])}

   ;; [:div {:class (c :flex :flex-row :items-baseline [:py 2] [:mb 4])}
   ;;  [:input#zd-search-input
   ;;   {:type "search"
   ;;    :placeholder ":title"
   ;;    :value search-text
   ;;    :class (c :border :outline-none [:w "100%"] [:rounded 4] :text-base [:px 2] [:py 1])}]]

   [:div
    [:a {:class utils/menu-link-c :href "/_search"}
     [:div {:class utils/menu-icon-c} [:i.fa-solid.fa-search]]
     [:div "Search"]]

    (let [cnt (count (or (store/errors ztx) []))]
      (when (> cnt 0)
        [:div#errors-link {:class (c :flex [:py 1.5] [:space-x 2] :items-center [:text :red-500])}
         [:div {:class (c :flex-1)} (utils/menu-link ztx 'errors)] [:b#errors-count {:class (c :text-sm)} cnt]]))

    [:a {:class utils/menu-link-c :href "/git"}
     [:div {:class utils/menu-icon-c} [:i.fa-solid.fa-timeline]]
     [:span "Timeline"]]

    [:a {:class utils/menu-link-c :href "/new"}
     [:div {:class utils/menu-icon-c} [:i.fa-solid.fa-circle-plus]]
     [:div "New Doc"]]]

   [:hr {:class (c [:my 4])}]
   (->> (store/menu ztx)
        (map (fn [doc]
               [:div {:class (c :flex :items-center :flex-row [:pseudo ":hover>a:last-child" :block] :justify-between)}
                (utils/menu-link ztx (:zd/docname doc))
                [:a {:class (c :cursor-pointer :text-lg [:text :gray-500] :hidden [:hover [:text :green-600]])
                     :href (str "/new?parent=" (:zd/docname doc))}
                 [:i.fas.fa-plus]]])))])
