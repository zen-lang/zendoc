(ns zd.datalog
  (:require [zen.core :as zen]
            [clojure.walk]
            [clojure.string :as str]
            [edamame.core]
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

(defn evict-by-query [ztx q]
  (doseq [res (query ztx q)]
    (evict ztx (str "'" (first res))))
  (xt/sync (:node (get-state ztx))))


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
    (doseq [[k sd] (:zd/subdocs doc)]
      (let [pid (get-in doc [:zd/meta :docname])
            id (str "'" pid  "." (name k))]
        (submit ztx (assoc (flatten-doc ztx sd) :xt/id id :parent (str "'" pid) :zd/subdoc true))))
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


(defn parse-query [q]
  (let [xs (->> (str/split q #"\n")
                (remove (fn [s] (or (str/blank? s) (str/starts-with? s "\\")))))
        columns   (->> xs
                       (filterv #(re-matches #"^\s?>.*" %))
                       (mapv #(subs % 1))
                       (mapv str/trim)
                       (filterv #(not (str/blank? %)))
                       (mapv (fn [x]
                               (if (str/starts-with? x "(")
                                 ['expr (edamame.core/parse-string x {:regex true})]
                                 (let [[e k] (str/split x #":" 2)]
                                   [(symbol e) (cond
                                                 (= k "*") (symbol k)
                                                 :else (keyword k))])))))
        index (atom {})
        find-items (->> (group-by first columns)
                        (reduce (fn [acc [k xs]]
                                  (if (= 'expr k)
                                    (->> (mapv second xs)
                                         (reduce (fn [acc e]
                                                   (swap! index assoc e (count acc))
                                                   (conj acc e))
                                                 acc))
                                    (let [cs (->> (mapv second xs) (dedupe) (into []))]
                                      (swap! index assoc k (count acc))
                                      (if (seq (filter (fn [x] (contains? #{'* :?} x)) cs))
                                        (conj acc (list 'pull k ['*]))
                                        (if (= cs [nil])
                                          (conj acc k)
                                          (conj acc (list 'pull k (mapv (fn [x] (if (nil? x) :xt/id x))cs))))))))
                                []))
        where-items
        (->> xs
             (filterv (every-pred #(not (str/ends-with? % " :asc"))
                                  #(not (str/ends-with? % " :desc"))
                                  #(not (re-matches #"^\s?>.*" %))))
             (mapv (fn [x] (let [res (edamame.core/parse-string (str/replace (str "[" x "]") #"#"  ":symbol/")
                                                                {:regex true})]
                             (cond
                               (list? (get res 1))
                               (vector res)

                               :else
                               res)
                             ))))
        where (->> where-items
                   (mapv (fn [x]
                           (clojure.walk/postwalk
                            (fn [y]
                              (if (and (keyword? y) (= "symbol" (namespace y)))
                                (str "'" (name y))
                                y)) x))))

        order-items
        (->> xs
             (filterv (every-pred #(or (str/ends-with? % " :asc")
                                       (str/ends-with? % " :desc"))
                                  #(not (re-matches #"^\s?>.*" %))))
             (mapv (fn [x] (edamame.core/parse-string (str/replace (str "[" x "]") #"#"  ":symbol/") {:regex true}))))

        order
        (->> order-items
             (mapv (fn [x]
                     (clojure.walk/postwalk
                       (fn [y]
                         (if (and (keyword? y) (= "symbol" (namespace y)))
                           (str "'" (name y))
                           y)) x))))]
    (into {:where where
           :order order
           :find find-items
           :columns columns
           :index @index} )))


(defn sugar-query [ztx q]
  (let [q (parse-query q)
        _ (println :q q)
        idx (:index q)
        res (->>
             (query ztx (dissoc q :columns :index))
             (mapv (fn [x]
                     (->> (:columns q)
                          (mapv (fn [[e c]]
                                  (cond
                                    (nil? c)  (or (get-in x [(get idx e) :xt/id]) (get-in x [(get idx e)]))
                                    (list? c) (get-in x [(get idx c)])
                                    (= c '*)  (get-in x [(get idx e)])
                                    (= c :?)  (keys (get-in x [(get idx e)]))
                                    :else     (get-in x [(get idx e) c]))))))))
        cols (->> (:columns q) (mapv second))]
    {:result res
     :query (dissoc q :columns :index)
     :columns cols}))
