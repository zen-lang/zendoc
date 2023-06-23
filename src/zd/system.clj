(ns zd.system
  (:require [zen.core :as zen]
            [zd.api :as api]))

(defn start [ztx]
  (zen/read-ns ztx 'zd)
  (zen/read-ns ztx 'zd.demo)
  (zen/start-system ztx 'zd.demo/system)
  (println :started)
  (println "http://localhost:4444"))

(defonce dtx (atom nil))

(defn -main [& opts]
  (let [ztx (zen/new-context {})]
    (reset! dtx ztx)
    (start ztx)))

(comment

  (-main)

  (zen/stop-system @dtx))
