(ns zd.view.doc
  (:require [zd.methods :as methods]
            [clojure.pprint]
            [clojure.string :as str]
            [zd.view.utils :as utils]
            [zd.view.topbar :as topbar]
            [stylo.core :refer [c]]
            [zd.store :as store]))

(defmethod methods/renderkey :title
  [ztx ctx {doc :doc title :data :as block}]
  [:h1 {:class (c :flex :items-center [:m 0] [:pt 2] [:pb 1] [:border-b :gray-400] [:mb 4] :text-2xl) :id "title"}
   (if-let [img (or (:avatar doc) (:logo doc))]
     [:img {:src img
            :class (c [:w 8] [:h 8] :inline-block [:mr 2] {:border-radius "100%"})}]
     (when-let [icon (or (:icon doc) (:zd/icon doc))]
       [:i {:class (str (str/join " " (map name icon)) " " (name (c [:mr 2] [:text :gray-600])))}]))
   title])

(defmethod methods/renderkey :desc
  [ztx ctx block]
  [:div {:class (c {:opacity 0.8})}
   (methods/rendercontent ztx ctx block)])

(defn render-edn [ztx ctx data]
  (cond
    (or (string? data) (number? data)) (pr-str data)
    (symbol? data) (utils/symbol-link ztx data)
    (keyword? data) [:span {:class (c [:text :green-700])} (str data)]

    (coll? data)
    (->> data
         (mapv (fn [x] [:div (render-edn ztx ctx x)]))
         (into [:div {:class (c :flex [:space-x 2] {:flex-wrap "wrap"})}
                [:div {:class (c [:text :gray-500] :text-sm)} "#"]]))
    :else
    [:div {:style {:background "white" :word-wrap "break-word"}}
     (if (string? data)
       data
       (with-out-str (clojure.pprint/pprint data)))]))

(defmethod methods/rendercontent :edn
  [ztx ctx {:keys [data] :as block}]
  (render-edn ztx ctx data))

(defmethod methods/renderann :table
  [ztx ctx {{headers :table} :ann k :key d :data cnt :content {cnt-type :zd/content-type} :ann :as block}]
  (let [pull-query? (and (= :datalog cnt-type) (set? cnt) (vector? (first cnt)) (map? (ffirst cnt)) (seq headers))
        edn-table? (and (= :edn cnt-type) (vector? d) (every? map? d))]
    [:div {:class (c [:py 4])}
     [:div {:class (c [:py 1] [:mb "0.8rem"] :border-b)}
      [:span {:class (c :uppercase)} ":"]
      [:a {:id k}
       [:span {:class (c :uppercase {:font-weight "600"})} k]]]
     (cond pull-query? (utils/table ztx ctx headers (map first cnt))
           edn-table?  (utils/table ztx ctx (set (mapcat keys d)) d)
           :else [:div
                  [:div (str "No table impl for " k ", " cnt-type)]
                  [:div (pr-str d)]])]))

(defmethod methods/renderann :link-badge
  [ztx ctx {data :data k :key}]
  [:div {:class (c :border [:m 1]  :inline-flex :rounded [:p 0])}
   [:a {:href data
        :target "_blank"
        :class (c :inline-block
                  [:bg :gray-100]
                  [:hover [:bg :gray-200]]
                  [:px 2]
                  [:py 0.5]
                  [:text "#4B5BA0"]
                  :text-sm
                  {:font-weight "400" :border-radius "4px 0 0  4px"})}
    k]])

(defmethod methods/renderann :badge
  [ztx ctx {key :key :as block}]
  [:div {:class (c :border [:my 1] [:mr 2] :inline-flex :items-center :rounded :shadow-sm)}
   [:div {:class (c :inline-block [:px 2] [:bg :gray-100] [:py 0.5] :text-sm [:text :gray-700]
                    :border-r
                    {:font-weight "400"  :border-radius "4px 0 0  4px"})}
    key]
   [:div {:class (c [:px 1] [:py 0.5] :inline-block :text-sm)}
    (methods/rendercontent ztx ctx block)]])

(defmethod methods/renderann :attribute
  [ztx ctx {k :key :as block}]
  [:div {:title "attribute" :class (c [:py 0.5] :flex :items-center [:space-x 4])}
   [:div {:class (c  [:text :gray-600] {:font-weight "500"})} k]
   [:div (methods/rendercontent ztx ctx block)]])

(defmethod methods/renderann :none
  [ztx ctx block])

(defmethod methods/renderann :hide
  [ztx ctx block])

(defmethod methods/renderann :default
  [ztx ctx {kp :key d :data :as block}]
  (let [basic-style (c [:pb 0.2] [:pt 1.5] [:mb 3] :text-lg :border-b)]
    [:div {:class (c [:py 4])}
     [:div {:class basic-style}
      [:a {:id kp :href (str "#" (name kp)) :class (c  {:font-weight "600"})} (name kp)]]
     ;; [:pre (pr-str annotations)]
     (methods/rendercontent ztx ctx block)]))

(defmethod methods/renderkey :default
  [ztx ctx block]
  (methods/renderann ztx ctx (update block :annotations (fn [ann]
                                                          (if (and (contains? #{:edn :str} (:type ann)) (nil? (:as ann)))
                                                            (assoc ann :as :badge)
                                                            ann)))))

(def h2 (c :text-xl :font-bold [:py 1] :border-b [:mt 2]))

(defn backlinks-block [ztx backlinks]
  ;; TODO move this logic into storage
  [:div
   ;; [:div {:class h2} "Backlinks:"]
   (->> backlinks
        (map (fn [[path docnames]]
               (when-let [docs (->> docnames (sort) (seq))]
                 [:div {:class (c [:mb 4])}
                  [:div {:class (c [:mb 2] [:pt 1] [:pb 0.5] :flex :items-center :border-b :font-bold [:text :gray-700] [:space-x 2])}
                   [:i.fa-solid.fa-arrow-left-to-line]
                   [:div (pr-str (first path))]]
                  (->> docs
                       (map (fn [docname]
                              [:div {:class (c [:py 1] {:border-bottom "1px dotted #f1f1f1"})}
                               (utils/menu-link ztx docname)])))]))))])

(declare document)

(defn localname [s]
  (last (str/split (str s) #"\.")))

(defn subdocs-block [ztx ctx subdocs]
  [:div
   ;; [:div {:class h2} "Subdocs:"]
   (->> subdocs
        (map (fn [doc]
               [:div {:class (c :border [:px 4] [:py 2] [:my 2] :rounded :shadow-sm)}
                [:a {:id (str "subdoc-" (:zd/docname doc))
                     :class (c :block :font-bold :border-b [:pt 1] [:pb 0.5] [:text :gray-600])}
                 "&" (localname (:zd/docname doc))]
                (document ztx ctx doc)])))])


(defn errors-view [ztx errors]
  [:div {:class (c [:text :red-700]  [:my 2] [:pb 2] :rounded :text-sm [:border :red-300])}
   [:ul {:class (c :font-bold [:mb 1] [:py 1] [:px 3] [:ml 0] [:text :red-600] [:bg :red-100] [:border-b :red-300])} "Document errors"]
   (for [err (sort-by :type errors)]
     [:li {:class (c [:py 0.5] :flex [:space-x 3] [:text :red-600] [:px 3])}
      [:span (pr-str (:path err))]
      [:span {:class (c [:ml 4] {:text-align "right"})} (:message err)]])])

(defn document [ztx ctx doc]
  [:div
   (->> (:zd/view doc)
        (map (fn [[k annotations]]
               (methods/renderkey ztx ctx {:doc doc
                                           :key k
                                           :data (get doc k)
                                           :annotations annotations}))))
   (when-let [backlinks (seq (:zd/backlinks doc))]
     (backlinks-block ztx backlinks))
   (when-let [subdocs (seq (:zd/subdocs doc))]
     (subdocs-block ztx ctx subdocs))
   #_(utils/pprint "doc" (dissoc doc :zd/view))])

(defn view [ztx ctx doc]
  [:div {:class (c [:w 200])}
   [:div (topbar/topbar ztx ctx doc)]
   (when-let [errors (seq (:zd/errors doc))]
     (errors-view ztx errors))
   (document ztx ctx doc)])

(defn preview [ztx ctx doc]
  [:div {:class (c [:w 200])}
   (when-let [errors (seq (:zd/errors doc))]
     (errors-view ztx errors))
   (document ztx ctx doc)])

;; TODO:  choose between badge and block based on (type: edn)-> badge; others -> block
