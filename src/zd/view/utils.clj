(ns zd.view.utils
  (:require
   [stylo.core :refer [c]]
   [zd.store :as store]
   [clojure.pprint]
   [zd.methods :as methods]
   [zd.zentext :as zentext]
   [zd.store :as store]
   [clojure.string :as str]))


(def btn-c (c :border :rounded [:py 1] [:px 2] [:bg :gray-200] :shadow-sm [:hover [:bg :gray-300]]))

(defn get-parent [ztx res]
  (when-let [nm (:zd/name res)]
    (let [pn (->> (str/split (str nm) #"\.")
                  (butlast)
                  (str/join "."))]
      (when-not (str/blank? pn)
        (or (store/get-doc ztx (symbol pn)) {:zd/name (symbol pn)})))))

(defn resolve-icon [ztx res]
  (if-let [ava (or (get-in res [:avatar]) (get-in res [:logo]))]
    {:type :img :img ava}
    (if-let [icon (or (get res :icon) (get res :zd/icon))]
      {:type :ico :icon icon}
      (when-let [parent (get-parent ztx res)]
        (resolve-icon ztx parent)))))

(def icon-c (name (c [:mr 1] )))

(defn icon [ztx res & [opts]]
  (let [icon-class (when-let [ic (:icon-class opts)] (name ic))]
    (if-let [icon (resolve-icon ztx res)]
      (cond (= (:type icon) :img)
            [:img {:src (:img icon)
                   :class (c :inline-block [:mr 1] [:h 4] [:w 4] :border {:border-radius "100%" :margin-bottom "1px"})}]
            (= (:type icon) :ico)
            [:i {:class (conj (map name (:icon icon)) icon-c icon-class)}])
      (when (:force-icon opts)
        [:i {:class ["fa-solid" "fa-file" icon-c icon-class]}]))))

(defn symbol-link [ztx s & [opts]]
  (if-let [res (store/get-doc ztx (symbol s))]
    (if (:zd/subdoc? res)
      (let [parent-name (get-in res [:zd/parent])
            parent (store/get-doc ztx parent-name)]
        [:a {:href (str "/" parent-name "#subdocs-" (:zd/docname res))
             :class (c :inline-flex :items-center [:text :blue-700] [:hover [:underline]] :whitespace-no-wrap)}
         (icon ztx res opts)
         (when-not (:compact opts) (or (:title res) (str (:title parent) " (" s ")")))])
      [:a {:href (str "/" s) :class (c :inline-flex :items-center [:text :blue-700] [:hover [:underline]] :whitespace-no-wrap)}
       (icon ztx res opts)
       (when-not (:compact opts)
         (or (:title res) s))])
    [:a {:href (str "/" s) :class (c [:text :red-600] [:bg :red-100]) :title "Broken Link"} s]))

(def menu-link-c (c :flex :items-center [:space-x 2] [:text :gray-700] [:py 1] [:hover [:text :blue-600] [:bg :gray-100]]))
(def menu-icon-c (c [:w 5] :text-center {:text-align "center"}))

(defn menu-link [ztx s & [opts]]
  (if-let [res (store/get-doc ztx (symbol s))]
    (if (:zd/subdoc? res)
      (let [parent-name (get-in res [:zd/parent])
            parent (store/get-doc ztx parent-name)]
        [:a {:href (str "/" parent-name "#subdocs-" (:zd/docname res))
             :class menu-link-c}
         [:div {:class menu-icon-c}
          (or (icon ztx res opts) [:i.fa-solid.fa-file])]
         [:div (when-not (:compact opts)
                 (str s " (" (str (:title parent) ) ")"))]])
      [:a {:href (str "/" s) :class menu-link-c}
       [:div {:class menu-icon-c}
        (or (icon ztx res opts) [:i.fa-solid.fa-file {:class (name (c [:text :gray-500]))}])]
       [:div
        (when-not (:compact opts)
          (or (:title res) s))]])
    [:a {:href (str "/" s) :class (c [:text :red-600] [:bg :red-100]) :title "Broken Link"} s]))

(defn pprint [title data]
  [:details {:class (c :text-xs [:mt 0.5])}
   [:summary {:class (c [:text :gray-500]) } title]
   [:pre {:class (c :border [:p 2] [:bg :gray-100])}
    (with-out-str (clojure.pprint/pprint data))]])

(defn table
  "renders table from vector of hashmaps. each hashmap is a memstore document"
  [ztx ctx headers data]
  [:div "TBD" [:table {:class (c :rounded [:py 2] [:w-max "80rem"] {:display "block" :table-layout "fixed"})}
    #_[:thead
       (->> headers
            (map (fn [k]
                   [:th {:class (c [:px 4] [:py 2] :border [:bg :gray-100])}
                    (str/lower-case (name k))]))
            (into [:tr]))]
    #_(->> data
           (mapv (fn [row]
                   [:tr
                    (doall
                     (for [h headers]
                       [:td {:class (c [:px 4] [:py 2] :border {:vertical-align "top"})}
                        (let [v (get row h)
                              docname (get row :xt/id)
                              doc (when docname (store/doc-get ztx (symbol docname)))
                              block {:key h :data v}]

                          (cond (= :xt/id h)
                                [:a {:href (str "/" docname)
                                     :class (c :inline-flex :items-center [:text "#4B5BA0"] [:hover [:underline]])}
                                 (icon ztx doc)
                                 (or (:title doc) docname)]

                                ;; (= :zentext (:zd/content-type key-ann))
                                ;; [:div {:class (c [:w-min "16rem"] :text-sm)}
                                ;;  (zentext/parse-block ztx v block)]

                                (= :edn (:zd/content-type key-ann))
                                (cond
                                  (set? v)
                                  (->> v
                                       (mapv (fn [e]
                                               (if (symbol? e)
                                                 (symbol-link ztx e)
                                                 (zentext/parse-block ztx (str e) block))))
                                       (into [:div {:class (c :flex :flex-col :text-sm {:flex-wrap "wrap"})}]))

                                  :else (methods/rendercontent ztx ctx block))

                                (some? v)
                                (methods/rendercontent ztx ctx {:data v :key h :ann {:zd/content-type :edn}})))]))]))
           (into [:tbody]))]])
