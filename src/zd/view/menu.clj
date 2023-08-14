(ns zd.view.menu
  (:require [zd.store :as store]
            [clojure.string :as str]
            [zd.view.utils :as utils]
            [stylo.core :refer [c]]))

(defn menu [ztx {{{search-text :search} :query-params :as req} :request r :root :as ctx} doc]
  [:div#left-nav {:class (c :border-r
                            [:bg "#fbfbfb"]
                            [:h "100vh"]
                            :overflow-y-auto
                            [:px 4]
                            :flex
                            :flex-col
                            :flex-grow
                            [:flex-shrink 0]
                            {:flex-basis "16rem"})}
   [:div {:class (c :flex :flex-row :items-baseline [:py 2])}
    [:input#zd-search-input
     {:type "search"
      :placeholder ":title"
      :value search-text
      :class (c :border :outline-none [:w "100%"] [:rounded 4] :text-base [:px 2] [:py 1])}]]
   (->> (store/menu ztx)
        (map (fn [doc]
               [:div {:class (c :flex [:py 1.5] :items-center :flex-row [:pseudo ":hover>a:last-child" :block] :justify-between)}
                (utils/menu-link ztx (:zd/docname doc))
                [:a {:class (c :cursor-pointer :text-lg [:text :gray-500] :hidden [:hover [:text :green-600]])
                     :href (str (:zd/docname doc) "." "_draft/edit")}
                 [:i.fas.fa-plus]]])))])

;; (if (not (str/blank? search-text))
;;   (let [query-result (map first (db/search ztx search-text #_(get-in doc [:zd/meta :docname]) #_page-number))]
;;     (if (seq query-result)
;;       [:div {:class (c :flex :flex-col [:pt 0] [:pr 6] [:pb 6] [:pl 6] {:flex-basis "18rem"})}
;;        (for [[i docname] (map-indexed vector query-result)]
;;          (let [{{anns :ann lu :last-updated} :zd/meta p :parent :as doc}
;;                (memstore/get-doc ztx (symbol docname))]
;;            [:div {:class (c [:py 2])}
;;             [:div {:class (c :overflow-hidden)}
;;              (link/symbol-link ztx docname)
;;              [:div {:class (c :flex :flex-row :items-baseline)}
;;               (when (symbol? p)
;;                 [:div p])
;;               (when (str/includes? (str docname) "_template")
;;                 [:span {:class (c [:text :orange-500] [:py 1] [:px 2])}
;;                  "_template"])
;;               (when (str/includes? (str docname) "_schema")
;;                 [:span {:class (c [:text :orange-500] [:py 1] [:px 2])}
;;                  "_schema"])]]]))]
;;       [:span {:class (c [:pt 0] [:pr 6] [:pb 6] [:pl 6] )}
;;        "No results"]))
;;   )

;; (let [grouped
;;       (->> (store/)
;;                 :docs
;;                 (group-by (comp :section #(nth % 2)))
;;                 (sort-by first)
;;                 (reverse))]
;;        [:div#zd-menu
;;         {:class (c [:pt 0] [:pr 6] [:pb 6] [:pl 6] [:pseudo ">div:last-child>#section-break" :hidden])}
;;         (for [[section docs] grouped]
;;           [:div
;;            (for [[d] docs]
;; )
           ;; [:div#section-break {:class (c :border-b [:my 2])}]])])
