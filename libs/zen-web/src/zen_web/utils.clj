(ns zen-web.utils
  (:require
   [clojure.string :as str]
   [zen.utils :as utils]
   [zen.core :as zen]))

(defn deep-merge
  "efficient deep merge"
  ([a b & more]
   (apply deep-merge (deep-merge a b) more))
  ([a b]
   (cond
     (and (map? a) (map? b))
     (loop [[[k v :as i] & ks] b, acc a]
       (if (nil? i)
         acc
         (let [av (get a k)]
           (if (= v av)
             (recur ks acc)
             (recur ks
                    (cond
                      (and (map? v) (map? av)) (assoc acc k (deep-merge av v))
                      (and (set? v) (set? av)) (assoc acc k (into v av))
                      (and (nil? v) (map? av)) (assoc acc k av)
                      :else (assoc acc k v)))))))
     (and (nil? a) (map? b)) b
     (and (nil? b) (map? a)) b
     :else
     ;; TODO pub zen event
     (do (println :error "deep-merge type missmatch: " a b) b))))

(defn content-type [hs]
  (when-let [ct (get hs "content-type")]
    (if-let [i (str/index-of ct ";")]
      (subs ct 0 i)
      ct)))

(defn resolve-mw [ztx sym]
  ;; take config from instance or engine
  (let [mw-cfg (zen/get-symbol ztx sym)]
    (->> (zen/engine-or-name mw-cfg)
         (zen/get-symbol ztx)
         (utils/deep-merge mw-cfg))))

