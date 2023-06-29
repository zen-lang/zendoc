(ns zd.methods
  (:require
   [clojure.pprint :as pprint]
   [clojure.string :as str]
   [stylo.core :refer [c]]
   [zd.methods :as methods]))

;; renders content of a block with :zd/content-type annotation
(defmulti rendercontent (fn [ztx ctx block]
                          (get-in block [:ann :zd/content-type])))

;; by default just pretty prints the content
(defmethod rendercontent :default
  [ztx ctx {:keys [data] :as block}]
  [:span (with-out-str (pprint/pprint data))])

;; renders key of a document with provided annotation
;; or by a block name
(defmulti renderkey (fn [ztx ctx block]
                      (:key block)))

(defn get-anns [block]
  (->> (:ann block)
       (remove (fn [[k _]]
                 (= "zd" (namespace k)))) ))

(defmulti renderann (fn [ztx ctx block]
                      ;; TODO pub error if more then 1 ann?
                      (when-let [[block-key _] (first (get-anns block))]
                        block-key)))

;; by default add a header and renders content of a block
(defmethod renderkey :default [ztx ctx {kp :key d :data anns :ann :as block}]
  ;; TODO fix render inline for bb run
  ;; TODO think if render inline is usable at all
  (let [render-inline?
        (and (not (:zd/multiline anns))
             (= (:zd/content-type anns) :edn)
             (not (map? d)))
        basic-style (c [:py 1] :border-b)
        embedded-style (c :flex :flex-row :items-center)
        multiline-embedded (c :flex :flex-row :items-baseline [:py 4])
        cnt (when-not (and (string? d) (str/blank? d))
              (rendercontent ztx ctx block))]
    (if (seq (get-anns block))
      (methods/renderann ztx ctx (assoc block :content cnt))
      [:div {:class (c [:py 4])}
       [:div {:class (if (:zd/render-subdoc? anns)
                       embedded-style
                       basic-style)}
        [:span {:class (c :uppercase)} ":"]
        [:a {:id kp}
         [:span {:class (c :uppercase {:font-weight "600"})} kp]]]
       cnt])))

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
