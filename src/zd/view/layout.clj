(ns zd.view.layout
  (:require
   [garden.core]
   [stylo.core :refer [c]]
   [stylo.rule :refer [join-rules]]
   [zd.view.menu]))

(defn c* [& args]
  (join-rules args))

;; TODO groom these global styles, delete unused css
(def common-style
  [:body {:font-family "sohne, \"Helvetica Neue\", Helvetica, Arial, sans-serif;" :padding "0" :margin "0"}
   [:img (c* {:display "inline-block"})]
   [:h1 (c* {:font-size "32px" :margin-top "10px" :margin-bottom "16px" :font-weight "600"})]
   [:h2 (c* :border-b {:font-size "24px" :margin-top "20px" :margin-bottom "14px" :font-weight "600" :line-height "30px"})]
   [:h3 (c* :border-b {:font-size "20px" :margin-top "20px" :margin-bottom "14px" :font-weight "600" :line-height "25px"})]
   [:h4 (c* {:font-size "16px" :margin-top "20px" :margin-bottom "14px" :font-weight "600" :line-height "20px"})]
   [:h5 (c* {:font-size "14px" :margin-top "20px" :margin-bottom "14px" :font-weight "600" :line-height "16px"})]
   [:.menu-item (c* :cursor-pointer [:hover [:bg :blue-700]])]
   [:.screenshot (c* :shadow-lg :border :rounded [:m 1] [:p 1])]
   [:.zd-search-popup
    (c* :border :rounded
        [:bg :white]
        [:p 4]
        :box-shadow {:z-index 100 :position "absolute"
                     :top "1rem"
                     :left "1rem"
                     :right "1rem"
                     :bottom "1rem"})
    [:.zd-search
     (c* :border :text-xl
         :rounded
         [:px 4] [:py 1]
         {:width "100%"})]
    [:.zd-comments (c* :text-sm [:text :gray-500] [:py 1])]
    [:.zd-results (c* [:mt 2])
     [:.zd-search-item (c* :cursor-pointer [:py 1] [:px 4]
                           :flex [:space-x 2] :items-center
                           [:hover [:bg :gray-200]] {:border "1px solid transparent"})
      [:&.current (c* [:bg :gray-200] :border)]
      [:i (c* [:text :gray-400])]
      [:.zd-search-desc (c* :text-sm [:text :gray-500])]]]]
   [:.zd-menu-item (c* :flex [:space-x 2] :items-center [:py 0.2]  :cursor-pointer [:px 6]
                       {:white-space "nowrap"})
    [:.zd-folder (c* [:text :transparent])]
    [:&:hover (c* [:bg :gray-200])
     [:.zd-folder.zd-empty (c* [:text :gray-500])]
     [:.zd-folder (c* [:text :orange-400])]]
    [:i (c* [:w 5] {:text-align "center"})]]
   [:ul (c* [:ml 4] [:mb 4])
    {:list-style "inside"
     :line-height "24px"}
    [:li {:display "list-item"}]
    [:ul (c* [:mb 0])]]
   [:ol (c* [:ml 4]
            {:list-style "disk inside"
             :line-height "24px"})
    [:li (c* [:my 1]
             {:display "list-item"
              :list-style "decimal"})]
    [:ol (c* [:ml 4])]]

   [:p (c* {:line-height "1.5rem"})]

   [:.hljs (c* [:bg :gray-100] :shadow-sm
               :border)]
   [:pre {:margin-top "1rem" :margin-bottom "1rem"}]
   [:.bolder (c* :font-bold)]
   [:.badge
    [:p {:margin 0}]]

   [:.visible {:visibility "visible"}]
   [:.pl-4  {:padding-left "1rem"}]
   [:.mindmap
    [:.node
     [:circle {:fill "#aaa"}]
     [:text {}]]
    [:.node--internal [:circle {:fill "#999"}]]
    [:.link {:fill "none"
             :stroke "#aaa"
             :stroke-opacity "0.4"
             :stroke-width "1.5px"}]]

   [:.zd-toggle [:.zd-content {:height "0"
                               :transform "scaleY(0) "
                               :transform-origin "top"
                               :transition "all 0.26s ease"}]]
   [:.zd-toggle.zd-open
    [:.zd-content {:transform "scaleY(1)"
                   :height "auto"}]
    [:.zd-block-title [:.fas {:transform "rotate(90deg)"
                              :transition "all 0.26s"}]]]])


(defn layout [ztx ctx content]
  [:html
   [:head
    [:style (stylo.core/compile-styles @stylo.core/styles)]
    [:style (garden.core/css common-style)]
    [:link {:rel "icon" :href "data:,"}]
    [:meta {:charset "UTF-8"}]
    [:link {:href "//cdnjs.cloudflare.com/ajax/libs/highlight.js/11.3.1/styles/default.min.css", :rel "stylesheet"}]
    [:link {:href "/static/js/fa/css/all.min.css", :rel "stylesheet"}]
    [:link  {:href "/static/js/spinner.css"  :rel "stylesheet"}]
    ;; TODO move scripts from head to body
    [:script {:src "//cdnjs.cloudflare.com/ajax/libs/highlight.js/11.3.1/highlight.min.js"}]
    [:script {:src "/static/js/mindmap.js"}]
    [:script {:src "https://cdnjs.cloudflare.com/ajax/libs/d3/7.8.5/d3.min.js"}]
    [:script {:src "//cdnjs.cloudflare.com/ajax/libs/highlight.js/11.3.1/languages/clojure.min.js"}]
    [:script {:src "//cdn.jsdelivr.net/npm/lodash@4.17.21/lodash.min.js"}]
    [:script {:src "/static/js/core.js"}]
    [:script {:src "https://cdn.jsdelivr.net/npm/mermaid@10.2.4/dist/mermaid.min.js"}]

    [:title "Here"]
    #_[:script {:src "/static/js/vega.min.js"}]
    [:script {:src "/static/js/quick-score.min.js"}]
    [:script {:src "/static/js/editor.js"}]]
   [:body {:class (c :overflow-hidden [:h "100vh"] [:text "#353B50"])}
    content
    [:script "mermaid.initialize({ startOnLoad: false});"]]])

(defn layout-with-menu [ztx ctx doc cnt]
  (layout ztx ctx
   [:div {:class (c :flex [:h "100%"])}
    (zd.view.menu/menu ztx ctx doc)
    [:div#page-content {:class (c :flex :flex-grow [:flex-shrink 1] [:py 6] [:px 12] :overflow-y-auto
                                  {:flex-basis "100%"})}
     cnt]]))
