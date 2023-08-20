(ns zd.store
  (:require [zen.core :as zen]
            [zd.parser]
            [xtdb.api :as xt]
            [clojure.walk]
            [clojure.java.io :as io]
            [zd.zentext]
            [zd.schema]
            [edamame.core]
            [clojure.set]
            [clojure.string :as str]))

;; TODO: add zen/events for plugins (like git)
;; doc/delete
;; doc/update
;; file/update
;; file/delete
;; TODO: support for macros
;; TODO: implement search
;; TODO: index of keys from props and existing keys
;; TODO: implement summary

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


(defn parse-query [q]
  (let [xs (->> (str/split q #"\n")
                (mapv str/trim)
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

        ;; TODO: fix order by
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
           :order-by order
           :find find-items
           :columns columns
           :index @index} )))


(defn datalog-sugar-query [ztx q]
  (let [q (parse-query q)
        idx (:index q)
        res (->>
             (datalog-query ztx (dissoc q :columns :index))
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

(defn *docname-to-path [docname]
  (-> (->> (str/split (str docname) #"\.") (str/join "/"))
      (str ".zd")))

(def docname-to-path (memoize *docname-to-path))

(defn *path-to-docname [path]
  (-> (str/replace path #"\.zd$" "")
      (str/split #"/")
      (->> (str/join "."))
      (symbol)))

(def path-to-docname (memoize *path-to-docname))

(defn child-docname [docname child]
  (symbol (str docname "." (name child))))

(defn parent-name [docname]
  (let [parts (str/split (str docname) #"\.")
        parent (butlast parts)]
    (if (= 'zd docname)
      'zd
      (if (empty? parent)
        'index
        (symbol (str/join "." parent))))))

(defn parent-dir [filename]
  (let [parts (str/split (str filename) #"/")
        parent (butlast parts)]
    (str/join "/" parent)))

;; (parent-name 'a.b.c)
;; (parent-name 'a)
;; (parent-name 'a.b)


(defn get-reference
  "return reference for docs and subdocs"
  [ztx docname])

(defn get-doc
  [ztx docname]
  (get-in @ztx [:zdb docname]))


(defn errors-clear [ztx docname]
  (swap! ztx update :zd/errors dissoc docname))

(defn backlinks-clear [ztx docname]
  (swap! ztx update :zd/backlinks
         (fn [bl]
           (->> bl
                (reduce (fn [bl [target refs]]
                          (let [refs' (reduce (fn [refs [k v]]
                                                (if (= k docname)
                                                  refs
                                                  (assoc refs k v)))
                                              {} refs)]
                            (if (seq refs')
                              (assoc bl target refs')
                              bl)))
                        {})))))

(defn delete-doc
  [ztx docname]
  (swap! ztx update :zdb dissoc docname))

(defn symbolize-subdocs [doc]
  (if-let [subdocs (seq (:zd/subdocs doc))]
    (assoc doc :zd/subdocs (mapv :zd/docname subdocs))
    doc))

(defn put-doc
  [ztx {docname :zd/docname :as doc}]
  (swap! ztx assoc-in [:zdb docname] (assoc doc :zd/parent (parent-name docname)))
  doc)

(defn walk-docs
  "call (f docname doc)"
  [ztx f]
  (doseq [[docname doc] (:zdb @ztx)]
    (f docname doc)))

(defn update-docs [ztx f]
  (doseq [[docname doc] (:zdb @ztx)]
    (if docname
      (put-doc ztx (f docname doc))
      (println :bad-doc doc))))

(defn put-errors [ztx docname errors]
  (if (seq errors)
    (swap! ztx assoc-in [:zd/errors docname] errors)
    (swap! ztx update :zd/errors dissoc docname)))

(defn clear-menu [ztx docname]
  (swap! ztx update :zd/menu dissoc docname))

(defn update-menu [ztx {docname :zd/docname :as doc}]
  (if-let [mo (or (:zd/menu-order doc) (:menu-order doc))]
    (swap! ztx update :zd/menu assoc  docname (assoc doc :zd/menu-order mo))
    (clear-menu ztx docname)))


(defn get-errors [ztx docname]
  (get-in @ztx [:zd/errors docname]))


(defn doc-validate
  "validate document"
  [ztx doc]
  (zd.schema/validate ztx (:zd/type doc) doc))


(defn validate-doc [ztx docname]
  (let [doc    (get-doc ztx docname)
        errors (doc-validate ztx doc)]
    (put-errors ztx docname errors)
    errors))

(defn re-validate
  "re-validate broken resources"
  [ztx]
  (swap! ztx assoc :zd/errors {})
  (walk-docs ztx (fn [docname _doc] (validate-doc ztx docname))))

(defn doc-inference
  "run inference"
  [ztx doc]
  doc)


(defn get-backlinks [ztx target]
  (->> (get-in @ztx [:zd/backlinks target])
       (reduce (fn [acc [docname attrs]]
                 (->> attrs
                      (reduce (fn [acc path]
                                (let [doc (get-doc ztx docname)
                                      ztype (:zd/type doc)]
                                  (if (not (and (:zd/subdoc? doc) (= target (:zd/parent doc))))
                                    (if (coll? ztype)
                                      (->> ztype
                                           (reduce (fn [acc ztype]
                                                     (update acc (if ztype (conj path ztype) path)
                                                             (fn [xs] (sort (conj (or xs []) docname)))))
                                                   acc))
                                      (update acc (if ztype (conj path ztype) path)
                                              (fn [xs] (sort (conj (or xs []) docname)))))
                                    acc)))
                              acc))) {})))

(defn backlinked [ztx docname]
  (->> (get-in @ztx [:zd/backlinks docname])
       (keys)
       (into #{})))



(defn to-property-name [nm]
  (let [parts  (str/split (str nm) #"\.")]
    (keyword (str/join "." (butlast parts)) (last parts))))

(to-property-name 'a.b)

(defn schema-clear [ztx docname]
  (swap! ztx update :zd/schema (fn [sch]
                                 (cond (get sch docname)
                                       (dissoc sch docname)))))

(defn get-type [doc]
  (when-let [tp (:zd/type doc)]
    (cond (symbol? tp) #{tp}
          (set? tp) tp
          :else nil)))

(defn update-schema [ztx doc]
  (cond (contains? (get-type doc) 'zd.class)
        (swap! ztx update-in [:zd/schema (:zd/docname doc)] (fn [x] (merge (or x {}) (select-keys doc [:zd/require :zd/summary]))))

        (contains? (get-type doc) 'zd.prop)
        (let [prop (to-property-name (:zd/docname doc))
              parent (:zd/parent doc)]
          (if parent
            (swap! ztx assoc-in  [:zd/schema parent :zd/props  prop] doc)
            (println :ERROR "Expected parent" doc)))))

(defn schema [ztx type-name]
  (get-in  @ztx [:zd/schema type-name]))


;; emit delete event
(defn doc-delete
  "delete document"
  [ztx docname]
  (let [doc (get-doc ztx docname)
        backlinks (backlinked ztx docname)]
    (datalog-delete ztx docname)
    (delete-doc ztx docname)
    (backlinks-clear ztx docname)
    (errors-clear ztx docname)
    (schema-clear ztx docname)
    ;; revalidate docs looking at this doc
    (->> backlinks (mapv #(validate-doc ztx %)))))

(defn to-doc
  "return docs from text representation"
  [ztx docname content & [{docpath :docpath lm :last-modified parent :zd/parent}]]
  (zd.parser/parse ztx docname content (cond-> {}
                                         parent  (assoc :zd/parent parent)
                                         docpath (assoc :zd/file docpath) lm (assoc :zd/last-modified lm))))

(defn file-content [ztx docname]
  (let [docpath (str (:zd/dir @ztx) "/" (docname-to-path docname))]
    (when (.exists (io/file docpath))
      (slurp docpath))))

;; TODO remove dir param
(defn file-read
  "read file and return vector of doc and subdocs"
  [ztx dir path & [opts]]
  (let [docpath (str dir "/" path)
        docname (path-to-docname path)
        content (slurp docpath)]
    (to-doc ztx docname content (merge {:docpath docpath :zd/parent (parent-name docname)} opts))))


(defn doc-get
  "get document from memory, validate, add backlinks etc"
  [ztx docname]
  (when-let [doc (get-doc ztx docname)]
    (let [errors (get-errors ztx docname)
          backlinks (get-backlinks ztx docname)
          subdocs (->> (:zd/subdocs doc)
                       (mapv #(doc-get ztx %)))]
      (when doc
        (cond-> (get-doc ztx docname)
          (seq errors)    (assoc :zd/errors errors)
          (seq backlinks) (assoc :zd/backlinks backlinks)
          (seq subdocs)   (assoc :zd/subdocs subdocs))))))

(defn doc-summary
  "return doc summary based on class"
  [ztx docname])

(defn summary
  "return doc summary based on class"
  [ztx schemaname]
  (zd.schema/summary ztx schemaname))

(defn edn-links [acc docname path node]
  (cond
    (and (not (= :zd/subdocs (first path)))  (symbol? node) (not (= node docname)))
    (update-in acc [node docname] (fnil conj #{}) path)

    (map? node)
    (reduce (fn [acc [k v]] (edn-links acc docname (conj path k) v)) acc node)

    (set? node)
    (reduce #(edn-links %1 docname path %2) acc node)

    (and (sequential? node) (not (list? node)))
    (->> (map-indexed vector node)
         (reduce (fn [acc [idx v]] (edn-links acc docname (conj path idx) v)) acc))

    :else acc))
(defn zentext-links [acc docname doc]
  (->> (:zd/view doc)
       (reduce (fn [acc [k {tp :type}]]
                 (if-let [s (and (= tp :zentext) (get doc k))]
                   (->> (zd.zentext/extract-links s)
                        (reduce (fn [acc l] (update-in acc [l docname] (fnil conj #{}) [k])) acc))
                   acc))
               acc)))

(defn collect-links [{docname :zd/docname :as doc}]
  (-> {}
      (edn-links docname [] doc)
      (zentext-links  docname doc)))

(defn update-backlinks [ztx doc]
  (let [links (collect-links doc)]
    (swap! ztx update :zd/backlinks (fn [ls] (merge-with merge ls links)))))

(defn update-keys-index [ztx doc]
  (swap! ztx update :zd/keys (fn [ks] (into (or ks #{}) (keys doc)))))




(defn re-index-doc [ztx {docname :zd/docname :as doc} & [{dont-validate :dont-validate}]]
  (put-doc ztx doc)
  (datalog-put ztx doc)
  (update-backlinks ztx doc)
  (update-menu ztx doc)
  (update-keys-index ztx doc)
  (update-schema ztx doc)
  (when (not dont-validate)
    (validate-doc ztx docname)))

;; emit save event
(defn doc-save
  "upsert document into memory databases & indexes"
  [ztx {docname :zd/docname :as doc} {dont-validate :dont-validate :as opts}]
  (let [idoc (doc-inference ztx doc)]
    (put-doc ztx idoc)
    (re-index-doc ztx idoc opts)
    idoc))

(defn children [ztx docname]
  (when-let [links (get-in @ztx [:zd/backlinks docname])]
    (->> links
         (reduce (fn [acc [doc props]]
                   (->> props
                        (reduce (fn [acc prop]
                                  (if (= [:zd/parent] prop)
                                      (conj acc doc)
                                      acc))
                                acc)))
                 #{}))))

(defn file-delete
  "save document content into file and recalculate databases"
  [ztx docname]
  (let [dir (:zd/dir @ztx)
        path (docname-to-path docname)
        docpath (str dir "/" path)
        doc (get-doc ztx docname)
        file (io/file docpath)
        filedir (io/file (str/replace docname #"\.zd$" ""))]
    (->> (:zd/subdocs doc)
         (mapv (fn [docname] (doc-delete ztx docname))))
    (doc-delete ztx docname)
    (clear-menu ztx docname)
    (->> (children ztx docname)
         (mapv (fn [child] (file-delete ztx child))))
    (when (.exists file) (.delete file))
    (when (.exists filedir) (.delete filedir))
    (->> (backlinked ztx docname)
         (mapv (fn [d] (validate-doc ztx d))))
    doc))

;; TODO: this dirty think a better way
(defn extract-docname [content]
  (let [lines (zd.parser/get-lines content)
        docname-line (->> lines (filter #(str/starts-with? % ":zd/docname")) (first))
        docname (when docname-line (when-let [s (-> docname-line (str/split #"\s+") (second))] (symbol s)))
        content' (->> lines
                      (remove #(str/starts-with? % ":zd/docname"))
                      (str/join "\n"))]
    [(when docname (symbol docname)) content']))

(extract-docname ":a 1\n:zd/docname docname\n:b 1")


(defn file-save
  "save document content into file and recalculate databases"
  [ztx docname content & [{dont-validate :dont-validate rename :rename :as opts}]]
  (let [[new-docname content] (extract-docname content)
        new-docname (or new-docname rename docname)
        dir         (:zd/dir @ztx)
        path        (docname-to-path new-docname)
        docpath     (str dir "/" path)
        doc         (to-doc ztx new-docname content {:docpath docpath :zd/parent (parent-name new-docname)})
        doc'        (symbolize-subdocs doc)]
    (when-let [old-doc (get-doc ztx docname)] 
      (if (not (= new-docname docname))
        (do
          (->> (children ztx docname)
               (mapv (fn [childname]
                       (let [new-childname (symbol (str new-docname (subs (str childname) (count (str docname)))))]
                         (file-save ztx childname (file-content ztx childname) {:rename new-childname})))))
          (file-delete ztx docname))
        (let [to-remove (clojure.set/difference (into #{} (:zd/subdocs old-doc)) (into #{} (:zd/subdocs doc')))]
          (->> to-remove (mapv #(doc-delete ztx %))))))
    (.mkdirs (io/file (parent-dir docpath)))
    (spit docpath content)
    (doc-save ztx doc' opts)
    (->> (:zd/subdocs doc)
         (mapv #(doc-save ztx % opts)))
    ;; fix broken links to this doc
    (->> (backlinked ztx new-docname)
         (mapv (fn [d] (validate-doc ztx d))))
    doc))

(defn dir-read
  "read docs from filesystem"
  [ztx & [dir]]
  (let [dir (or dir (:zd/dir @ztx))
        dir (io/file dir)
        dir-path (.getPath dir)
        docs (->> (file-seq dir)
                  (map (fn [f]
                         (let [p (.getPath f)]
                           (when (and (str/ends-with? p ".zd") (.exists f))
                             (let [path (subs p (inc (count dir-path)))]
                               (file-read ztx dir-path path {:last-modified (.lastModified f)}))))))
                  (filter identity))]
    docs))

(defn load-meta [ztx]
  (let [doc (-> (to-doc ztx 'zd (slurp (io/resource "zd.zd")) {:zd/parent 'zd})
                (assoc  :zd/readonly true :zd/parent 'zd))]
    (put-doc ztx (symbolize-subdocs doc))
    (->> (:zd/subdocs doc)
         (mapv #(put-doc ztx %)))
    (put-doc ztx {:zd/docname 'errors
                  :zd/view [ [:title] [:zd/all-errors]]
                  :zd/icon [:fa-solid :fa-triangle-exclamation]
                  :title "Errors"
                  :zd/all-errors true})))

(defn dir-load
  "read docs from filesystem and load into memory"
  [ztx & [dir]]
  (let [dir (or dir (:zd/dir @ztx))]
    (load-meta ztx)
    (->> (dir-read ztx dir)
         (mapv (fn [doc]
                 (put-doc ztx (symbolize-subdocs doc))
                 (->> (:zd/subdocs doc)
                      (mapv #(put-doc ztx %))))))
    (update-docs ztx
                 (fn [_docname doc]
                   (let [idoc (doc-inference ztx doc)]
                     (re-index-doc ztx idoc))))))

(defn menu
  "return navigation"
  [ztx]
  (->> (vals (get @ztx :zd/menu))
       (sort-by (fn [x]
                  (let [mo (:zd/menu-order x)]
                    [(if (number? mo) mo 100) (str (:zd/docname x))])))))

(defn errors
  "return all errors"
  [ztx]
  (get @ztx :zd/errors))


(defn breadcrump [ztx docname]
  (let [parts (str/split (str docname) #"\.")]
    (loop [[p & ps] parts nm [] acc []]
      (if (nil? p)
        acc
        (recur ps (conj nm p) (conj acc (symbol  (str/join "." (conj nm p)))))))))

(defn props
  "return all used props (keys)"
  [ztx]
  (->> (:zd/keys @ztx)
       (mapv (fn [k] {:name (str k)}))))


(defn annotations [ztx] )

(defn symbols [ztx]
  (->> (:zdb @ztx)
       (mapv (fn [[k {old-ico :icon  ico :zd/icon logo :logo tit :title}]]
               {:title tit
                :name k
                :logo logo
                :icon (or old-ico ico)}))))

(defn search [ztx query]
  (let [parts (str/split (str/lower-case (str/trim (or query ""))) #"\s+")
        regex (re-pattern (str/join ".*" parts))]
    (->> (:zdb @ztx)
         (filter (fn [[k {t :title d :desc :as doc}]]
                   (and k (re-find regex (str/lower-case (str t " " k " " d))))))
         (map (fn [[id doc]]
                 (-> doc (assoc :zd/docname id))))
         (sort-by :zd/docname)
         (take 30))))
