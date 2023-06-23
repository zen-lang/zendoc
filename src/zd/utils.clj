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
