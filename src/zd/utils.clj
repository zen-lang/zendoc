(ns zd.utils
  (:require
   [zen.core :as zen]))

(defn safecall [ztx f err]
  (fn [& args]
    (try (let [r (apply f args)]
           {:type :zd.utils/safecall :result r})
         (catch Exception e
           (let [err* (assoc err :message (.getMessage e) #_:trace #_(.getStackTrace e))]
             (zen/pub ztx 'zd.events/safecall-error {:err err*})
             {:type :zd.utils/safecall :error err*})))))

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
     (and (nil? b) (nil? a)) nil
     :else
     (do (println :error "deep-merge type missmatch: " a b) b))))
