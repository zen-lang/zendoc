(ns zd.link
  (:require
   [stylo.core :refer [c]]
   [zd.memstore :as memstore]
   [clojure.string :as str]))

(defn get-parent [ztx res]
  (when-let [nm (:zd/name res)]
    (let [pn (->> (str/split (str nm) #"\.")
                  (butlast)
                  (str/join "."))]
      (when-not (str/blank? pn)
        (or (memstore/get-doc ztx (symbol pn))
            {:zd/name (symbol pn)})))))

(defn resolve-icon [ztx res]
  (if-let [ava (or (get-in res [:avatar]) (get-in res [:logo]))]
    {:type :img :img ava}
    (if-let [icon (get res :icon)]
      {:type :ico :icon icon}
      (when-let [parent (get-parent ztx res)]
        (resolve-icon ztx parent)))))

(defn icon [ztx res]
  (when-let [icon (resolve-icon ztx res)]
    (cond (= (:type icon) :img)
          [:img {:src (:img icon)
                 :class (c :inline-block [:mr 1]
                           [:h 4] [:w 4]
                           :border
                           {:border-radius "100%"
                            :margin-bottom "1px"})}]
          (= (:type icon) :ico)
          [:i {:class (str (str/join " " (map name (:icon icon))) " " (name (c [:mr 1] [:text :gray-500])))}])))

(defn symbol-link [ztx s & [opts]]
  (if-let [res (memstore/get-doc ztx (symbol s))]
    [:a {:href (str "/" s) :class (c :inline-flex :items-center [:text "#4B5BA0"] [:hover [:underline]] :whitespace-no-wrap)}
     (icon ztx res)
     (when-not (:compact opts)
       (or (:title res) s))]
    (let [parts (str/split (str s) #"\.")
          ss (str/join "." (butlast parts))
          sub (last parts)]
      (if-let [sres (when-let [p (memstore/get-doc ztx (symbol ss))]
                      (get-in p [:zd/subdocs (keyword sub)]))]
        [:a {:href (str "/" ss "#subdocs-" sub) :class (c :inline-flex :items-center [:text "#4B5BA0"] [:hover [:underline]] :whitespace-no-wrap)}
         (icon ztx sres)
         (when-not (:compact opts) (or (:title sres) s))]
        [:a {:href (str "/" s) :class (c [:text :red-600] [:bg :red-100]) :title "Broken Link"} s]))))
