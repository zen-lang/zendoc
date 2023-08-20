
(ns zd.schema)

(defn infere [ztx schema doc]
  )

(defmulti validate-rule (fn [ztx errors rule-name rule-value k v] rule-name))
(defmulti validate-type (fn [ztx errors type-name k v] type-name))

(defmethod validate-rule :default
  [ztx errors rule-name rule-value k v]
  (println :unknown rule-name)
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

(defn add-schema [ztx docname schema]
  (swap! ztx assoc-in [:zd/schema docname] schema)
  schema)

;; TODO: fix this hack with empty schema
(defn get-schema [ztx docnames]
  (if (nil? docnames)
    [{}]
    (let [docnames (if (symbol? docnames) #{docnames} docnames)]
      (-> (->> docnames
               (map (fn [docname]
                      (get-in @ztx [:zd/schema docname])))
               (filter identity)
               (seq))
          (or [{}])))))


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
(defn validate [ztx schemaname doc]
  (let [schemas (get-schema ztx schemaname)]
    (->> schemas
         (reduce (fn [errors schema]
                   (let [errors (->> (:zd/require schema)
                                     (reduce (fn [errors prop]
                                               (if (not (contains? doc prop))
                                                 (conj errors {:type :required
                                                               :message (str prop " is required")
                                                               :path [prop]})
                                                 errors))
                                             errors))]
                     (->> doc
                          (reduce (fn [errors [k v]]
                                    (let [errors (if-let [prop-schema (get-in schema [:zd/props k])]
                                                   (validate-prop ztx errors prop-schema k v)
                                                   errors)
                                          errors (validate-refs ztx errors k v)]
                                      errors))
                                  errors))))
                 [])
         seq)))

(defn summary [ztx schema & [doc]]
  (when-let [summary (get-in @ztx [:zd/schema schema :zd/summary])]
    (if doc 
      (select-keys doc summary)
      summary)))
