(ns zd.view.doc
  (:require [zd.methods :as methods]
            [clojure.pprint]
            [clojure.string :as str]
            [zd.zentext :as zentext]
            [stylo.core :refer [c]]))


(defmethod methods/rendercontent :zentext
  [ztx ctx {:keys [data] :as block}]
  [:div (zentext/parse-block ztx data block)])

;; embedded-style (c :flex :flex-row :items-center)
;; render-inline? (and (not (:zd/multiline anns)) (= (:zd/content-type anns) :edn) (not (map? d)))
;; multiline-embedded (c :flex :flex-row :items-baseline [:py 4])
(defmethod methods/renderkey :default
  [ztx ctx {kp :key d :data doc :doc annotations :annotations :as block}]
  (let [basic-style (c [:pb 0.2] [:pt 1.5] [:mb 3] :text-lg :border-b)
        cnt (with-out-str (clojure.pprint/pprint d))]
    ;; (methods/renderann ztx ctx (assoc block :content cnt))
    [:div {:class (c [:py 4])}
     [:div {:class basic-style}
      [:a {:id kp :href (str "#" (name kp)) :class (c  {:font-weight "600"})} (name kp)]]
     (methods/rendercontent ztx ctx block)]))

(defn view [ztx ctx doc]
  [:div
   (->> (:zd/view doc)
        (map (fn [[k annotations]]
                (methods/renderkey ztx ctx {:doc doc
                                            :key k
                                            :data (get doc k)
                                            :annotations annotations}))))])
