(ns zd.methods
  (:require
   [hiccup.core :as hiccup]
   [clojure.pprint :as pprint]
   [clojure.string :as str]
   [stylo.core :refer [c]]
   [zd.memstore]
   [zd.link :as link]
   [zd.methods :as methods]))

;; renders content of a block with :zd/content-type annotation
(defmulti rendercontent (fn [ztx ctx block] (get-in block [:ann :zd/content-type])))

;; by default just pretty prints the content
(defmethod rendercontent :default
  [ztx ctx {:keys [data] :as block}]
  [:span (with-out-str (pprint/pprint data))])

;; renders key of a document with provided annotation
;; or by a block name
(defmulti renderkey (fn [ztx ctx block]
                      (:key block)))

(defmethod renderkey :menu-order [& args])
(defmethod renderkey :icon [& args])
(defmethod renderkey :zd/icon [& args])
(defmethod renderkey :zd/menu-order [& args])

(defn get-anns [block]
  (->> (:ann block)
       (remove (fn [[k _]]
                 (= "zd" (namespace k)))) ))

(defmulti renderann (fn [ztx ctx block]
                      ;; TODO pub error if more then 1 ann?
                      (when-let [[block-key _] (first (get-anns block))]
                        block-key)))

(defmethod renderkey :desc
  [ztx ctx {kp :key d :data anns :ann :as block}]
  [:p {:class (c [:text :gray-600])}
   (rendercontent ztx ctx block)])

;; by default add a header and renders content of a block
(defmethod renderkey :default
  [ztx ctx {kp :key d :data anns :ann :as block}]
  ;; TODO fix render inline for bb run
  ;; TODO think if render inline is usable at all
  (let [basic-style (c [:pb 0.2] [:pt 1.5] [:mb 3] :text-lg :border-b)
        embedded-style (c :flex :flex-row :items-center)
        render-inline? (and (not (:zd/multiline anns)) (= (:zd/content-type anns) :edn) (not (map? d)))
        multiline-embedded (c :flex :flex-row :items-baseline [:py 4])
        cnt (when-not (and (string? d) (str/blank? d)) (rendercontent ztx ctx block))]
    (if (seq (get-anns block))
      (methods/renderann ztx ctx (assoc block :content cnt))
      [:div {:class (c [:py 4])}
       [:div {:class (if (:zd/render-subdoc? anns) embedded-style basic-style)}
        [:a {:id kp :href (str "#" (name kp)) :class (c  {:font-weight "600"})}
         (name kp)]]
       ;; TODO think about rendering flow
       (try (hiccup/html cnt) (catch Exception e (with-out-str (pprint/pprint cnt))))])))

(defmethod renderkey :errors-view
  [ztx ctx {d :data :as block}]
  [:div {:class (c)}
   (for [[docname errors] (->> (zd.memstore/get-all-errors ztx)
                               (sort-by #(str (first %))))]
     [:div {:class (c [:py 1] :border-b)}
      [:b (str docname) " > "(link/symbol-link ztx docname)]
      (for [e errors]
        [:div {:class (c :flex [:space-x 3] :borer-b)}
         [:div "⚬ " (str (:type e))]
         [:div (str (:path e))]
         [:div (:message e)]])])])

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
