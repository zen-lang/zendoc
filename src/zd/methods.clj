(ns zd.methods
  (:require
   [hiccup.core :as hiccup]
   [clojure.pprint :as pprint]
   [clojure.string :as str]
   [stylo.core :refer [c]]
   [zd.methods :as methods]))

(defmulti annotation (fn [name params] name))

(defmethod annotation :badge [nm params] {:as :badge})

(defmethod annotation :link-badge [nm params] {:as :badge})

(defmethod annotation :hide [nm params] {:as :none})

(defmethod annotation :block [nm params] {:as :block})

(defmethod annotation :attribute [nm params] {:as :attribute})

(defmethod annotation :default [nm params] (assoc {} nm params))


(defmulti do-parse (fn [ctx tp s] tp))
(defmethod do-parse :default [ctx _tp s] (str/trim s))

;; renders content of a block with :zd/content-type annotation
(defmulti rendercontent (fn [ztx ctx block] (or (get-in block [:annotations :type])
                                               (type (:data block)))))

(defmethod rendercontent :default
  [ztx ctx {data :data annotations :annotations doc :doc :as block}]
  [:div
   [:b  (pr-str annotations)]
   [:span (with-out-str (pprint/pprint data))]])

(defmulti renderkey (fn [ztx ctx {data :data annotations :annotations doc :doc :as block}] (:key block)))

(defmethod renderkey :menu-order [& args])
(defmethod renderkey :icon [& args])
(defmethod renderkey :zd/icon [& args])
(defmethod renderkey :zd/menu-order [& args])

(defn get-anns [block]
  (->> (:ann block)
       (remove (fn [[k _]]
                 (= "zd" (namespace k)))) ))

(defmulti renderann (fn [ztx ctx block] (get-in block [:annotations :as])))

;; zentext methods
(defmulti inline-method   (fn [ztx m arg ctx] (keyword m)))
(defmulti inline-function (fn [ztx m arg ctx] (keyword m)))
(defmulti process-block   (fn [ztx tp args cnt] tp))

;; renders UI widget that is updated asynchronously
(defmulti widget (fn [ztx ctx page & [opts]] (keyword (:widget ctx))))

(defmethod widget
  :default
  ;; TODO add ctx to args
  [ztx wgt page & [opts]]
  [:div {:class (c [:text :red-500])}
   ;; draw error status bar
   "Widget - " (pr-str wgt) " is not implemented"])

;; evaluates a macro during the document loading phase
(defmulti eval-macro! (fn [ztx doc docpath [expr & args]] expr))

(defmethod eval-macro! :default
  [ztx doc docpath macro]
  {:error {:message (str (pr-str macro) " implementation not found")
           :type "macro-notfound"
           :docpath docpath}})

(comment
  (ns-unmap *ns* 'renderkey)

  )
