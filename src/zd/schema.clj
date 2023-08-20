(ns zd.schema
  (:require [clojure.string :as str]))


(defn to-keyname [docname]
  (let [parts (str/split (str docname) #"\.")]
    (keyword (str/join "." (butlast parts)) (last parts))))

;; TODO: implement other API
(defn add-class [ztx {docname :zd/docname :as doc}]
  (swap! ztx assoc-in [:zd/classes docname] doc)
  doc)

(defn remove-class [ztx docname]
  (swap! ztx update :zd/classes dissoc docname)
  docname)

(defn get-class [ztx docname]
  (get-in @ztx [:zd/classes docname]))

(defn add-prop [ztx {docname :zd/docname :as doc}]
  (let [keyname (to-keyname docname)]
    (swap! ztx assoc-in [:zd/props keyname] doc)
    doc))

(defn remove-prop [ztx docname]
  (let [keyname (to-keyname docname)]
    (swap! ztx update :zd/props dissoc keyname)
    docname))

(defn get-prop  [ztx keyname]
  (get-in @ztx [:zd/props keyname]))


(defn infere [ztx schema doc]
  )

(defmulti validate-rule (fn [ztx errors rule-name rule-value k v] rule-name))
(defmulti validate-type (fn [ztx errors type-name k v] type-name))

(defmethod validate-rule :default
  [ztx errors rule-name rule-value k v]
  errors)

(defmethod validate-type :default
  [ztx errors type-name k v]
  (println :unknown/type type-name)
  errors)

(defmethod validate-type 'zd.string
  [ztx errors _ k v]
  (if (and v (not (string? v)))
    (conj errors {:type :type :path [k] :message (str "Expected string, got " (type v))})
    errors))

(defmethod validate-type 'zd.number
  [ztx errors _ k v]
  (if (and v (not (number? v)))
    (conj errors {:type :type :path [k] :message (str "Expected number, got " (type v))})
    errors))

(defmethod validate-type 'zd.int
  [ztx errors _ k v]
  (if (and v (not (int? v)))
    (conj errors {:type :type :path [k] :message (str "Expected int, got " (type v))})
    errors))

(defmethod validate-type 'zd.symbol
  [ztx errors _ k v]
  (if (and v (not (symbol? v)))
    (conj errors {:type :type :path [k] :message (str "Expected symbol, got " (type v))})
    errors))

(defmethod validate-rule :zd/data-type
  [ztx errors _rule-name rule-value k v]
  (validate-type ztx errors rule-value k v))

(defn validate-prop [ztx errors prop-schema k v]
  (->> prop-schema
       (reduce (fn [errors [rule-name rule-value]]
                 (validate-rule ztx errors rule-name rule-value k v))
               errors)))


;; TODO: fix this hack with empty schema
(defn get-schema [ztx doc]
  (when-let [docnames (:zd/type doc)]
    (let [docnames (if (symbol? docnames) #{docnames} docnames)]
      (->> docnames
           (map (fn [docname] (get-class ztx docname)))
           (filter identity)
           (seq)))))


(defn validate-refs [ztx errors k v]
  (cond (and (symbol? v) (not (get-in @ztx [:zdb v])))
        (conj errors {:type :reference :message (str "`" v "` not found") :path [k]})

        (set? v)
        (->> v
             (reduce (fn [errors v]
                       (if (and (symbol? v) (not (get-in @ztx [:zdb v])))
                         (conj errors {:type :reference :message (str "`" v "` not found") :path [k]})
                         errors))
                     errors))
        :else errors))

;; TODO one walk over data (not per schema)
(defn validate [ztx doc]
  (let [schemas (get-schema ztx doc)
        errors (->> schemas
                    (reduce (fn [errors schema]
                              (->> (:zd/require schema)
                                   (reduce (fn [errors prop]
                                             (if (not (contains? doc prop))
                                               (conj errors {:type :required
                                                             :message (str prop " is required")
                                                             :path [prop]})
                                               errors))
                                           errors)))
                            []))]
    (->> doc
         (reduce (fn [errors [k v]]
                   (let [errors (if-let [prop-schema (get-prop ztx k)]
                                  (validate-prop ztx errors prop-schema k v)
                                  errors)
                         errors (validate-refs ztx errors k v)]
                     errors))
                 errors))))


(defn summary [ztx schema & [doc]]
  (when-let [summary (get-in @ztx [:zd/classes schema :zd/summary])]
    (if doc
      (select-keys doc summary)
      summary)))
