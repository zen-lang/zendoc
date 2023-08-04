(ns zd.datalog
  (:require [zen.core :as zen]
            [clojure.walk]
            [clojure.string :as str]
            [xtdb.api :as xt]))

(defn get-state [ztx]
  (get-in @ztx [:zen/state :datalog :state]))

(defn submit [ztx data]
  (if-let [{n :node} (get-state ztx)]
    (xt/submit-tx n [[::xt/put data]])
    :no/xtdb))

(defn evict [ztx data]
  (if-let [{n :node} (get-state ztx)]
    (xt/submit-tx n [[::xt/evict data]])
    :no/xtdb))

(defn query [ztx query & params]
  (if-let [{n :node} (get-state ztx)]
    (clojure.walk/postwalk
     (fn [x] (if (and (string? x) (str/starts-with? x "'"))
              (symbol (subs x 1))
              x))
     (apply xt/q (xt/db n) query params))
    :no/xtdb))

(defn flatten-doc [ztx {{dn :docname :as m} :zd/meta :as doc}]
  (let [meta (->> m
                  (map (fn [[k v]] [(keyword "meta" (name k)) v]))
                  (into {}))
        doc (-> (dissoc doc :zd/backlinks :zd/subdocs :zd/meta)
            (merge meta)
            (assoc :xt/id (str "'" (:docname m))))]
    (clojure.walk/postwalk (fn [x] (if (symbol? x) (str "'" x) x)) doc)))

;; TODO rename to zd.datalog
(defmethod zen/op 'zd/query
  [ztx config params & [session]]
  (query ztx params))

(defmethod zen/op 'zd/submit
  [ztx _config params & [_session]]
  (submit ztx params))

(defmethod zen/op 'zd.events/datalog-sync
  [ztx _config {_ev :ev doc :params} & [_session]]
  (let [xtdb-doc (flatten-doc ztx doc)
        result (submit ztx xtdb-doc)]
    ;; TODO und where does result go in pub/sub
    result))

(defmethod zen/op 'zd.events/datalog-delete
  [ztx _config {{dn :docname} :params} & [_session]]
  (cond
    (or (string? dn) (symbol? dn)) (evict ztx (str dn))))

(defmethod zen/start 'zd.engines/datalog
  [ztx {zd-config :zendoc :as config} & opts]
  (let [{r :root} (zen/get-symbol ztx zd-config)]
    {:config config
     :root r
     :node (xt/start-node {:xtdb.lucene/lucene-store {}})}))

(defmethod zen/stop 'zd.engines/datalog
  [ztx config {n :node}]
  (.close n))
