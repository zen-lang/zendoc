;; TODO move to core
(ns zd.system
  (:require [zen.core :as zen]
            [zd.datalog]
            [zd.api :as api]))

(defn restart [ztx]
  (zen/stop-system ztx)
  (zen/read-ns ztx 'zd)
  (zen/read-ns ztx 'zd.demo)
  (zen/start-system ztx 'zd.demo/system)
  (println :started)
  (println "http://localhost:4444"))

(defonce dtx (atom nil))

(defn -main [& opts]
  (let [ztx (zen/new-context {})]
    (reset! dtx ztx)
    (restart ztx)))

(defn query [ztx q]
  (zd.datalog/query ztx q))

(comment

  (-main)

  ;; TODO: fix document update
  ;; TODO: add validation by class
  ;; TODO: add annotations by prop

  (zen/stop-system ztx)
  (def ztx (zen/new-context {}))
  (restart ztx)


  (query ztx '{:where [[e :xt/id id]]
               :find [(pull e [:xt/id :title])]})

  (zd.memstore/get-doc ztx 'needs)

  (:zrefs @@dtx)

  )
