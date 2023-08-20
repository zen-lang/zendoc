(ns zd.view.zentext
  (:require
   [zd.view.utils :as link]
   [stylo.core :refer [c]]
   [clojure.string :as str]
   [zd.zentext :as zentext]
   [zd.methods :as methods]))

(defn github-disc [d]
  [:a {:href (str "https://github.com/fhir-ru/core/discussions/" d)
       :target "_blank"
       ;; TODO make default color profile in macro css
       :class (c [:text "#4B5BA0"] [:space-x 0.5])}
   [:i.fa-regular.fa-comments
    {:class (name (c [:text :blue-400] :text-sm))}]
   [:span d]])

(defmethod methods/inline-method :github/disc
  [ztx m d ctx]
  (github-disc d))

(defmethod methods/inline-method :d
  [ztx m d ctx]
  (github-disc d))


(defmethod methods/inline-method :symbol-link
  [ztx m s ctx]
  (link/symbol-link ztx s))

(defmethod methods/inline-method :mention
  [ztx m s ctx]
  (link/symbol-link ztx (symbol (str "people." s))))

(defmethod methods/inline-method :bold
  [ztx m s ctx]
  [:b s])

(defmethod methods/inline-method :italic
  [ztx m s ctx]
  [:i s])

(defmethod methods/inline-method :x
  [ztx m s ctx]
  [:i.fa-solid.fa-square-check {:class (name (c [:text :green-600]))}])

(defmethod methods/inline-method :fa
  [ztx m s ctx]
  (let [cls (->>
             (str/split s #"\s")
             (mapv str/trim)
             (remove str/blank?)
             (mapv (fn [x] (if (str/starts-with? x ":") (subs x 1) x)))
             (str/join " "))]
    [:i {:class cls}]))

(defmethod methods/inline-method :md/link
  [ztx m s ctx]
  (let [[txt href] (str/split s #"\]\(" 2)]
    [:a {:href href :target :_blank :class (c [:text "#4B5BA0"] [:hover [:underline]])} txt]))

(defmethod methods/inline-method :md/img
  [ztx m s ctx]
  (let [[txt href] (str/split s #"\]\(" 2)]
    [:img {:src href :alt txt}]))

(defmethod methods/inline-method :code
  [ztx m s ctx]
  [:code {:class (c [:px 1.5] [:py 0.5] [:bg :gray-100] [:text :pink-600]
                    :text-sm
                    {:border-radius "4px"
                     :font-family "ui-monospace,SFMono-Regular,SF Mono,Menlo,Consolas,Liberation Mono,monospace"})}
   s])

(defmethod methods/inline-method :b
  [ztx m s ctx]
  [:b s])

(defn img-src [ctx src]
  (when src
    (cond (str/starts-with? src "http") src
          (str/starts-with? src "/") (str "/static" src)
          :else (str "/static/" src))))

(defmethod methods/inline-method
  :img
  [ztx m arg ctx]
  (let [[src alt] (str/split arg #"\s+" 2)
        src (img-src ctx src)]
    [:img {:src src :alt alt}]))

(defmethod methods/inline-function
  :img
  [ztx m args ctx]
  (let [[src opts & _]  args
        src (img-src ctx src)]
    [:img (assoc opts :src src)]))

(defmethod methods/inline-function
  :echo
  [ztx m args ctx]
  [:span "((" m (pr-str args) "))"])

;; TODO fix this zentext function
#_(defmethod methods/inline-function
  :resource
  [ztx m [sym & path] ctx]
  (if-let [sym (zd.db/get-resource ztx sym)]
    (get-in sym path)
    [:div "Could not find " (pr-str sym)]))

(defmethod methods/inline-method :a
  [ztx m arg ctx]
  (let [[src text] (str/split arg #"\s+" 2)]
    [:a {:href src :target :_blank :class (c [:text "#4B5BA0"] [:hover [:underline]])}
     " " (or text src)]))

(defmethod methods/inline-method
  :src
  [ztx m arg ctx]
  [:a {:class (c [:text :green-600] :title "TODO")}
   (str arg)])

(defmethod methods/inline-method :default
  [ztx m arg ctx]
  [:span {:class (c [:text :red-600] [:bg :red-100])} (str "No inline-method for " m " arg:" arg)])

(defmethod methods/process-block "code" [ztx _ lang cnt]
  [:div.code-block
   [:pre {:class (c :text-sm)
          :style {:position "relative" :white-space "pre-wrap"}}
    [:code {:style {:word-wrap "break-word"} :class (str "language-" lang " hljs")} cnt]]])

(defmethod methods/process-block :default [ztx tp args cnt]
  [:pre {:params args :tp tp}
   [:code.hljs cnt]])

(defmethod methods/process-block "table" [ztx _ _ args]
  (let [[cols & rows] (->> (str/split-lines args) (mapv (fn [x] (str/split x #"\|"))))]
    [:table {:class (c :shadow-sm :rounded)}
     [:thead
      (into [:tr] (->> cols (mapv (fn [k] [:th {:class (c [:px 4] [:py 2] :border [:bg :gray-100])} k]))))]
     (into [:tbody]
           (->> rows
                (mapv (fn [x]
                        (into [:tr]
                              (->> x (mapv (fn [v] [:td {:class (c [:px 4] [:py 2] :border)} v]))))))))]))

(defmethod methods/rendercontent :zentext
  [ztx ctx {:keys [data] :as block}]
  [:div (zentext/parse-block ztx data block)])
