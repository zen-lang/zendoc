(ns zd.view.timeline
  (:require
   ;; [zd.runner :as runner]
   [stylo.core :refer [c]]
   [zd.methods :as methods]
   [zd.view.utils :as utils :refer [btn-c]]
   [clojure.string :as str]))

(defn file-to-link [ztx f]
  (when (str/starts-with? f "docs/")
    (utils/symbol-link ztx
     (-> (str/replace f #"^docs/" "")
         (str/replace #"\.zd$" "")
         (str/replace #"/" ".")
         (symbol)))))

(defn changes-view [ztx changes]
  [:form {:action "/git" :method "POST"}
   [:div {:class (c :font-bold :text-lg :border-b :flex [:space-x 4] [:py 0.5] :divide-y)}
    [:div {:class (c :flex-1)} "Changes"]]
   [:textarea {:class (c [:my 4] :border [:px 4] :block [:py 2] :rounded {:width "100%"})
               :name "comment"
               :placeholder "Comment"}]
   [:div
    (->> changes
         (map (fn [{m :change f :file}]
                [:div {:class (c [:py 0.5]  :flex :items-baseline [:space-x 4])}
                 [:input {:type "checkbox" :name (str f) :value "true"}]
                 [:div {:class (c [:w 3])}
                  (get {:deleted   [:i.fa-solid.fa-trash]
                        :new       [:i.fa-solid.fa-square-plus]
                        :modified  [:i.fa-solid.fa-pen-to-square]} m (str m))]
                 [:div f]
                 (file-to-link ztx f)])))]
   [:div {:class (c [:my 2] [:py 2] :border-t)}
    [:button {:class btn-c} "Commit"]]])

(defn history-view [ztx history]
  [:div {:class (c [:mt 4])}
   [:div {:class (c :font-bold :text-lg :border-b)} "History"]
   (->> history
        (map-indexed
         (fn [i {date :date commits :commits}]
           [:details  {:class (c [:my 4]) :open (> 10 i)}
            [:summary {:class (c :block :border-b [:pt 1] [:pb 0.5] :font-bold [:text :gray-600] :cursor-pointer [:hover [:bg :gray-100]])} date]
            [:div
             (->> commits
                  (map-indexed (fn [j com]
                                 [:details {:class (c [:py 0]) :open (> 3 i)}
                                  [:summary {:class (c  [:px 2] [:bg :gray-100] :border-b [:space-x 4] :cursor-pointer [:hover [:bg :gray-100]])}
                                   [:span {:class (c [:w 8] [:text :gray-600])}
                                    (last (str/split (:time com) #"\s"))]
                                   [:span (:user com)]
                                   [:span "-"]
                                   [:span (:comment com)]]
                                  [:div {:class (c [:pt 2] [:pb 4] :border-b)}
                                   (->> (:files com)
                                        (map (fn [f]
                                               [:div {:class (c [:ml 4] [:py 0.5] :flex :items-center [:space-x 2])}
                                                [:i.fa-regular.fa-file {:class (str (c [:gray-400]))}]
                                                [:space f]
                                                (file-to-link ztx f)])))]])))]])))])

(defn view [ztx {changes :changes history :history}]
  [:div {:class (c [:w 200])}
   [:h1 {:class (c :flex :items-center {:border-bottom "1px solid #ddd"})}
    [:div {:class (c :flex-1)} "Timeline"]
    [:button {:class btn-c} "Pull Changes"]]
   (when (seq changes)
     (changes-view ztx changes))
   (when (seq history)
     (history-view ztx history))])
