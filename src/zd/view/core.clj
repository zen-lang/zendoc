(ns zd.view.core
  (:require
   [stylo.core :refer [c]]
   [zd.store :as store]
   [clojure.string :as str]))

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

(def icon-c (name (c [:mr 1] [:text :gray-500])))

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
    (if (get-in res [:zd/meta :subdoc])
      (let [parent (get-in res [:zd/parent])]
        [:a {:href (str "/" parent "#subdocs-" (:zd/name res))
             :class (c :inline-flex :items-center [:text "#4B5BA0"] [:hover [:underline]] :whitespace-no-wrap)}
         (icon ztx res opts)
         (when-not (:compact opts) (or (:title res) s))])
      [:a {:href (str "/" s) :class (c :inline-flex :items-center [:text "#4B5BA0"] [:hover [:underline]] :whitespace-no-wrap)}
       (icon ztx res opts)
       (when-not (:compact opts)
         (or (:title res) s))])
    [:a {:href (str "/" s) :class (c [:text :red-600] [:bg :red-100]) :title "Broken Link"} s]))

(defn menu [ztx])

(defn content [ztx doc])

(defn doc [ztx doc])

(defn page [ztx doc])

(defn editor [ztx doc])

(defn errors [ztx doc])
