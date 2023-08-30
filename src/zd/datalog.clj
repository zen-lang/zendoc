(ns zd.datalog
  (:require [xtdb.api :as xt]
            [clojure.walk]
            [clojure.string :as str]
            [edamame.core]))


(defmulti add-instruction (fn [acc k v] k))

(defmethod add-instruction :limit
  [acc k v]
  (if (int? v)
    (assoc acc :limit v)
    acc))

(defmethod add-instruction :desc
  [acc k v]
  (-> acc
      (update :order-by (fn [x] (conj (or x []) [v :desc])))
      (update :columns (fn [x]  (conj (or x []) {:name v :hidden true})))))

(defmethod add-instruction :asc
  [acc k v]
  (-> acc
      (update :order-by (fn [x] (conj (or x []) [v :asc])))
      (update :columns (fn [x]  (conj (or x []) {:name v :hidden true})))))

(defmethod add-instruction :default
  [acc k v]
  (println :unknown/instruction k v)
  acc)

(defn parse-instruction [acc l]
  (let [[k v] (str/split (str/trim (subs l 1)) #"\s+" 2)
        k (str/trim k)
        v (try (edamame.core/parse-string v) (catch Exception _e (println :datalog.edn/error v) v))]
    (if k
      (add-instruction acc (keyword k) v)
      acc)))

(defn symbolize [x]
  (clojure.walk/postwalk
   (fn [y]
     (if (and (keyword? y) (= "symbol" (namespace y)))
       (str "'" (name y))
       y)) x))

(defn parse-condition [acc x]
  (let [res (edamame.core/parse-string (str/replace (str "[" x "]") #"#"  ":symbol/") {:regex true})]
    (update acc :where conj (symbolize res))))

(defn parse-select [acc x]
  (let [x (str/trim (subs x 1))
        [x lbl] (mapv str/trim (str/split x #"\|" 2))]
    (if (str/starts-with? x "(")
      (update acc :columns conj {:name 'expr
                                 :expr (edamame.core/parse-string x {:regex true})
                                 :label (or lbl x)})
      (let [[e k] (str/split x #":" 2)
            k  (cond (= k "*") (symbol k) :else (keyword k))]
        (update acc :columns conj {:name (symbol e)
                                   :prop k
                                   :label (or lbl e)})))))

(defn auto-columns [query]
  (if (and (seq (:where query)) (empty? (:columns query)))
    (assoc query :columns
           (->> (:where query)
                (reduce (fn [fnd expr]
                          (->> expr
                               (reduce (fn [fnd s] (if (symbol? s)
                                                    (conj fnd {:name s :label (str s)})
                                                    fnd))
                                       fnd)))
                        [])))
    query))

#_(->> (group-by :name (:columns query))
     (reduce (fn [acc [k xs]]
               (if (= 'expr k)
                 (->> (mapv :expr xs)
                      (reduce (fn [acc e]
                                (swap! index assoc e (count acc))
                                (conj acc e))
                              acc))
                 (let [cs (->> (mapv :prop xs) (dedupe) (into []))]
                   (swap! index assoc k (count acc))
                   (if (seq (filter (fn [x] (contains? #{'* :?} x)) cs))
                     (conj acc (list 'pull k ['*]))
                     (if (= cs [nil])
                       (conj acc k)
                       (conj acc (list 'pull k (mapv (fn [x] (if (nil? x) :xt/id x))cs))))))))
             []))

(defn make-find [query]
  (let [index (atom {})
        fnd   (->> (:columns query)
                   (mapv (fn [{nm :name prop :prop expr :expr}]
                           (cond
                             (contains? #{'* :?} prop) (list 'pull nm ['*])
                             prop (list 'pull nm [prop])
                             expr expr
                             :else nm))))]
    (assoc query :find fnd :index @index)))

(defn parse-query [q]
  (let [query (loop [[l & ls] (str/split q #"\n")
                     acc {:where [] :columns []}]
                (if (and (nil? l) (empty? ls))
                  acc
                  (let [l (str/trim l)]
                    (cond
                      (str/blank? l) (recur ls acc)
                      (str/starts-with? l "//") (recur ls acc)
                      (str/starts-with? l ">") (recur ls (parse-select acc l))
                      (str/starts-with? l "<") (recur ls (parse-instruction acc l))
                      :else (recur ls (parse-condition acc l))))))]
    (-> query
        (auto-columns)
        (make-find))))

;; dir - one
;; index.zd - fixed entry point
;; everything is sync

(defn get-db [ztx]
  (if-let [db (:db @ztx)]
    db
    (let [db (xt/start-node {})]
      (swap! ztx assoc :db db)
      db)))

(defn encode-query [q]
  (clojure.walk/postwalk (fn [x] (if (and (list? x) (= 'quote (first x)))
                                  (str "'" (second x))
                                  x)) q))

(defn encode-data [q]
  (clojure.walk/postwalk (fn [x] (if (symbol? x) (str "'" x) x)) q))

(defn decode-data [res]
  (clojure.walk/postwalk
   (fn [x] (if (and (string? x) (str/starts-with? x "'")) (symbol (subs x 1)) x))
   res))

(defn datalog-put [ztx data]
  (assert (:zd/docname data) (pr-str data))
  (let [db (get-db ztx)
        data (if (:xt/id data) data (assoc data :xt/id (:zd/docname data)))
        res (xt/submit-tx db [[::xt/put (encode-data (dissoc data :zd/docname :zd/view))]])]
    (xt/sync db)
    res))

(defn datalog-delete [ztx docname]
  (let [db (get-db ztx)
        res (xt/submit-tx db [[::xt/evict (str "'" docname)]])]
    (xt/sync db)
    res))

(defn datalog-get [ztx id]
  (let [db (get-db ztx)]
    (decode-data (xt/entity (xt/db db) (str "'" id)))))

;; cache based on database status
(defn datalog-query
  "run datalog query"
  [ztx query & params]
  (let [db (get-db ztx)]
    (-> (apply xt/q (xt/db db) (encode-query query) params)
        (decode-data))))

;; (cond
;;   (nil? c)  (or (get-in x [(get idx e) :xt/id]) (get-in x [(get idx e)]))
;;   (list? c) (get-in x [(get idx c)])
;;   (= c '*)  (get-in x [(get idx e)])
;;   (= c :?)  (keys (get-in x [(get idx e)]))
;;   :else     (get-in x [(get idx e) c]))

(defn datalog-sugar-query [ztx q]
  (try
    (let [q   (parse-query q)
          res (->> (datalog-query ztx (dissoc q :columns :index))
                   (mapv (fn [x]
                           (loop [[{prop :prop :as c} & cs] (:columns q)
                                  i 0
                                  acc []]
                             (if (and (nil? c) (empty? cs))
                               acc
                               (cond (:hidden c)
                                     (recur cs (inc i) acc)
                                     (= :* prop)
                                     (recur cs (inc i) (conj acc (get x i)))
                                     (= :? prop)
                                     (recur cs (inc i) (conj acc (keys (get x i))))
                                     prop
                                     (recur cs (inc i) (conj acc (get-in x [i prop])))
                                     :else
                                     (recur cs (inc i) (conj acc (get x i)))))))))]
      {:result  res
       :query   (dissoc q :columns :index)
       :columns (->> (:columns q) (remove :hidden) (mapv :label))})
    (catch Exception e
      {:error (.getMessage e)})))
