(ns zd.schema
  (:require [clojure.pprint :refer [pprint]]
            [clojure.string :as str]))

(defn to-keyname [docname]
  (let [parts (str/split (str docname) #"\.")
        ns (str/join "." (butlast parts))]
    ;; TODO rename to _schema.zd ?
    (if (= ns "_")
      (keyword (last parts))
      (keyword ns (last parts)))))

;; TODO: implement other API
(defn add-class [ztx {docname :zd/docname :as doc}]
  (swap! ztx assoc-in [:zd/classes docname] doc)
  doc)

(defn remove-class [ztx docname]
  (swap! ztx update :zd/classes dissoc docname)
  docname)

(defn get-class-props [ztx classname]
  ;; TODO impl superclass check
  (let [{sch-order :zd/subdocs dn :zd/docname :as parent-sch}
        (get-in @ztx [:zdb (or (get-in @ztx [:zdb classname :zd/child-type]) classname)])
        sch-props
        (->> (:zd/props @ztx)
             (filter (fn [[k {parent :zd/parent}]]
                       (= parent dn)))
             (sort-by (fn [[k {prop-name :zd/docname}]]
                        (.indexOf sch-order prop-name))))

        {gl-order :zd/subdocs :as global-sch} (get-in @ztx [:zdb '_])
        global-props
        (->> (:zd/props @ztx)
             (filter (fn [[k {parent :zd/parent}]]
                       (= parent '_)))
             (sort-by (fn [[k {prop-name :zd/docname}]]
                        (- (.indexOf gl-order prop-name)))))]
    (into sch-props global-props)))

(defn get-class-template [ztx classname]
  (->> (get-class-props ztx classname)
       (map (fn [[k {df :zd/default dt :zd/data-type}]]
              (when (some? df)
                (cond
                  (and (= dt 'zd.string) (str/blank? df))
                  (str k " \"\"")

                  (= 'zd.string dt)
                  (format (str k " \"%s\"") df)

                  (= 'zd.zentext dt)
                  (format (str k " /\n%s") df)

                  :else
                  (format (str k " %s") df)))))
       (str/join "\n")))

(defn get-class [ztx docname]
  (or (get-in @ztx [:zd/classes docname])
      (get-in @ztx [:zdb docname])))

(defn add-prop [ztx {docname :zd/docname :as doc}]
  (let [keyname (to-keyname docname)]
    (swap! ztx assoc-in [:zd/props keyname] doc)
    (swap! ztx update :zd/keys (fn [ks] (conj (or ks #{}) keyname)))
    doc))

(defn remove-prop [ztx docname]
  (let [keyname (to-keyname docname)]
    (swap! ztx update :zd/props dissoc keyname)
    docname))

(defn get-prop  [ztx keyname]
  (get-in @ztx [:zd/props keyname]))

(defn infere [ztx doc]
  (if-let [tp (and (nil? (:zd/type doc))
                   (when-let [p (:zd/parent doc)]
                     (or (get-in @ztx [:zdb p :zd/child-type])
                         (get-in @ztx [:zd/classes p :zd/child-type]))))]
    (-> doc
        (assoc :zd/type tp)
        (update :zd/infered (fn [x] (conj (or x []) :zd/type))))
    doc))

(defmulti validate-rule (fn [ztx errors rules rule-name rule-value k v] rule-name))
(defmulti validate-type (fn [ztx errors rules type-name k v] type-name))

(defmethod validate-rule :default
  [ztx errors rules rule-name rule-value k v]
  errors)

(defmethod validate-type :default
  [ztx errors rules type-name k v]
  (println :unknown/type type-name)
  errors)

(defmethod validate-type 'zd.string
  [ztx errors rules _ k v]
  (if (and v (not (string? v)))
    (conj errors {:type :type :path [k] :message (str "Expected string, got " (type v))})
    errors))


(defmethod validate-type 'zd.boolean
  [ztx errors rules _ k v]
  (if (and v (not (boolean? v)))
    (conj errors {:type :type :path [k] :message (str "Expected boolean, got " (type v))})
    errors))

(defmethod validate-type 'zd.number
  [ztx errors rules _ k v]
  (if (and v (not (number? v)))
    (conj errors {:type :type :path [k] :message (str "Expected number, got " (type v))})
    errors))

(defmethod validate-type 'zd.int
  [ztx errors rules _ k v]
  (if (and v (not (int? v)))
    (conj errors {:type :type :path [k] :message (str "Expected int, got " (type v))})
    errors))

(defmethod validate-type 'zd.symbol
  [ztx errors rules _ k v]
  (if (and v (not (symbol? v)))
    (conj errors {:type :type :path [k] :message (str "Expected symbol, got " (type v))})
    errors))

(defmethod validate-type 'zd.date
  [ztx errors rules _ k v]
  (if (and v (not (inst? v)))
    (conj errors {:type :type :path [k] :message (str "Expected inst, got " (type v))})
    errors))

(defmethod validate-type 'zd.keyword
  [ztx errors rules _ k v]
  (if (and v (not (keyword? v)))
    (conj errors {:type :type :path [k] :message (str "Expected keyword, got " (type v))})
    errors))

(defmethod validate-rule :zd/data-type
  [ztx errors rules _rule-name rule-value k v]
  (cond
    (and (:zd/maybe-set? rules) (set? v))
    (->> v (reduce (fn [errors v] (validate-type ztx errors rules rule-value k v)) errors))

    (and (:zd/maybe-vector? rules) (sequential? v))
    (->> v (reduce (fn [errors v] (validate-type ztx errors rules rule-value k v)) errors))

    (:zd/set? rules)
    (if (not (set? v))
      (conj errors {:type :type :path [k] :message (str "Expected set, got " (type v))})
      (->> v (reduce (fn [errors v] (validate-type ztx errors rules rule-value k v)) errors)))

    (:zd/vector? rules)
    (if (not (sequential? v))
      (conj errors {:type :type :path [k] :message (str "Expected vector, got " (type v))})
      (->> v (reduce (fn [errors v] (validate-type ztx errors rules rule-value k v)) errors)))

    :else
    (validate-type ztx errors rules rule-value k v)))

(defn validate-prop [ztx errors prop-schema k v]
  (->> prop-schema
       (reduce (fn [errors [rule-name rule-value]]
                 (validate-rule ztx errors prop-schema rule-name rule-value k v))
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
