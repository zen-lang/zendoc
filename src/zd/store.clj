(ns zd.store
  (:require [zen.core :as zen]
            [zd.parser]
            [xtdb.api :as xt]
            [clojure.walk]
            [clojure.java.io :as io]
            [zd.zentext]
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

(defn datalog-simple-query [ztx query]
  "run datalog simple query"
  )

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

(defn parent-name [docname])

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

(defn put-doc
  [ztx doc]
  (swap! ztx assoc-in [:zdb (:zd/docname doc)] doc)
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

(defn validate-refs [ztx doc]
  (->> doc
       (reduce
        (fn [acc [k v]]
          (if (symbol? v)
            (if-not (get (:zdb @ztx) v)
              (conj acc {:type :reference :message (str "'" v " not found") :path [k]})
              acc)
            (if (set? v)
              (->> v
                   (reduce (fn [acc x]
                             (if (and (symbol? x) (not (get (:zdb @ztx) x)))
                               (conj acc {:type :reference :message (str "'" x " not found") :path [k]})
                               acc))
                           acc))
              acc)))
        [])))

(defn doc-validate
  "validate document"
  [ztx doc]
  (let [cls (when-let [tp (:zd/type doc)] (if (set? tp) tp #{tp}))
        errors
        (->> cls
             (mapcat
              (fn [cn]
                (let [c (get (:zdb @ztx) cn)]
                  (->> (:zd/require c)
                       (reduce (fn [acc k]
                                 (if (contains? doc k)
                                   acc
                                   (conj acc {:type :require :message "require" :path [k] :schema cn})))
                               []))))))
        errors (into errors (into (validate-refs ztx doc)))]
    errors))


(defn validate-doc [ztx docname]
  (let [doc (get-doc ztx docname)]
    (put-errors ztx docname (doc-validate ztx doc))))

(defn re-validate
  "re-validate broken resources"
  [ztx]
  (swap! ztx assoc :zd/errors {})
  (walk-docs ztx (fn [docname _doc] (validate-doc ztx docname))))

(defn doc-inference
  "run inference"
  [ztx doc]
  doc)



(defn get-backlinks [ztx docname]
  (get-in @ztx [:zd/backlinks docname]))

;; emit delete event
(defn doc-delete
  "delete document"
  [ztx docname]
  (let [backlinks (get-backlinks ztx docname)]
    (datalog-delete ztx docname)
    (delete-doc ztx docname)
    (backlinks-clear ztx docname)
    (errors-clear ztx docname)
    ;; revalidate docs looking at this doc
    (->> backlinks
         (mapv (fn [[d _]] (validate-doc ztx d))))))

(defn to-docs
  "return docs from text representation"
  [ztx docname content & [{docpath :docpath lm :last-modified}]]
  (let [[doc & subdocs] (cond->>
                            (zd.parser/parse ztx docname content)
                          docpath (mapv (fn [x] (cond-> x
                                                 docpath (assoc :zd/file docpath)
                                                 lm (assoc :zd/last-modified lm)))))]
    (if (seq subdocs)
      (into [(assoc doc :zd/subdocs (into #{} (mapv :zd/docname subdocs)))] subdocs)
      [doc])))

(defn file-read
  "read file and return vector of doc and subdocs"
  [ztx dir path & [opts]]
  (let [docpath (str dir "/" path)
        docname (path-to-docname path)
        content (slurp docpath)]
    (to-docs ztx docname content (merge {:docpath docpath} opts))))

(defn doc-get
  "get document from memory, validate, add backlinks etc"
  [ztx docname]
  (let [errors (get-errors ztx docname)
        backlinks (get-backlinks ztx docname)
        doc (get-doc ztx docname)]
    (when doc
      (cond-> (get-doc ztx docname)
        (seq errors) (assoc :zd/errors errors)
        (seq backlinks) (assoc :zd/backlinks backlinks)))))

(defn doc-summary
  "return doc summary based on class"
  [ztx docname])

(defn edn-links [acc docname path node]
  (cond
    (symbol? node)
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

;; emit save event
(defn doc-save
  "upsert document into memory databases & indexes"
  [ztx {docname :zd/docname :as doc} {dont-validate :dont-validate}]
  (let [idoc (doc-inference ztx doc)]
    (put-doc ztx idoc)
    (datalog-put ztx idoc)
    (update-backlinks ztx idoc)
    (update-menu ztx idoc)
    (when-not dont-validate
      (validate-doc ztx docname))
    idoc))

(defn file-delete
  "save document content into file and recalculate databases"
  [ztx docname]
  (let [dir (:zd/dir @ztx)
        path (docname-to-path docname)
        docpath (str dir "/" path)
        doc (get-doc ztx docname)
        file (io/file docpath)]
    (->> (:zd/subdocs doc)
         (mapv (fn [docname] (doc-delete ztx docname))))
    (doc-delete ztx docname)
    (clear-menu ztx docname)
    (when (.exists file) (.delete file))))

;; check that it is identical to what you have now
;; and skeep if so
(defn file-save
  "save document content into file and recalculate databases"
  [ztx docname content & [{dont-validate :dont-validate :as opts}]]
  (let [dir (:zd/dir @ztx)
        path (docname-to-path docname)
        docpath (str dir "/" path)]
    (file-delete ztx docname)
    (spit docpath content)
    (->> (to-docs ztx docname content {:docpath docpath})
         (mapv #(doc-save ztx % opts)))))

(defn dir-read
  "read docs from filesystem"
  [ztx dir]
  (let [dir (io/file dir)
        dir-path (.getPath dir)
        docs (->> (file-seq dir)
                  (mapcat (fn [f]
                            (let [p (.getPath f)]
                              (when (str/ends-with? p ".zd")
                                (let [path (subs p (inc (count dir-path)))]
                                  (file-read ztx dir-path path {:last-modified (.lastModified f)})))))))]
    docs))

(defn dir-load
  "read docs from filesystem and load into memory"
  [ztx dir]
  (->> (dir-read ztx dir)
       (mapv (fn [doc] (put-doc ztx doc))))
  (update-docs ztx
               (fn [_docname doc]
                 (let [idoc (doc-inference ztx doc)]
                   (update-menu ztx idoc)
                   (datalog-put ztx idoc)))))

(defn menu
  "return navigation"
  [ztx]
  (->> (vals (get @ztx :zd/menu))
       (sort-by :zd/menu-order)))

(defn errors
  "return all errors"
  [ztx]
  (get @ztx :zd/errors))


(defn symbol-search   [ztx query])
(defn keywords-search [ztx query])
(defn search          [ztx query])
