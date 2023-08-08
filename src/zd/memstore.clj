(ns zd.memstore
  (:require
   [zd.meta :as meta]
   [zd.methods :as methods]
   [zd.macros]
   [zen.core :as zen]
   [zd.reader :as reader]
   [clojure.string :as str]
   [zd.utils :as u]))


(defn validate-doc [ztx doc]
  (if-let [cls (when-let [tp (:zd/type doc)] (if (set? tp) tp #{tp}))]
    (let [errors
          (->> cls
               (mapcat
                (fn [cn]
                  (let [c (get (:zdb @ztx) cn)]
                    (->> (:zd/require c)
                         (reduce (fn [acc k]
                                   (if (contains? doc k)
                                     acc
                                     (conj acc
                                           {:type :doc-validation
                                            :message (str " required by " cn)
                                            :path [k]})))
                                 []))))))]
      (if (seq errors)
        (assoc-in doc [:zd/meta :errors] errors)
        doc))
    doc))

(defn get-doc [ztx nm]
  (let [backlinks (get (:zrefs @ztx) nm)
        doc (validate-doc ztx (get (:zdb @ztx) nm))]
    (when doc (assoc doc :zd/backlinks backlinks))))




(defn *edn-links [acc docname path node]
  (cond
    (symbol? node)
    (update-in acc [node docname] (fnil conj #{}) path)

    (map? node)
    (reduce (fn [acc [k v]]
              (*edn-links acc docname (conj path k) v))
            acc
            node)

    (set? node)
    (reduce #(*edn-links %1 docname path %2)
            acc
            node)

    (and (sequential? node) (not (list? node)))
    (->> (map-indexed vector node)
         (reduce (fn [acc [idx v]]
                   (*edn-links acc docname (conj path idx) v))
                 acc))

    :else acc))

(defn edn-links [acc docname path cnt]
  (*edn-links acc docname path cnt))

(def exclude #{\? \. \! \; \,})

(defn find-symbols [ch cnt]
  (loop [acc (seq cnt)
         syms #{}]
    (if (nil? acc)
      syms
      (let [[l r] (reader/split #(= % ch) acc)
            [le tail] (reader/split #(or (= % \space)
                                         (= % \newline))
                                    r)]
        (cond
          (nil? r) syms
          (not= (last l) \space) (recur tail syms)
          :else
          (let [sym (->> (rest le)
                         (reverse)
                         (drop-while #(contains? exclude %))
                         (reverse)
                         (apply str)
                         symbol)]
            (recur tail (conj syms sym))))))))

(defn zentext-links [acc docname path cnt]
  (let [links (find-symbols \# cnt)
        ;; zentext mention @ appends people.prefix
        mentions (map (fn [m] (symbol (str "people." m)))
                      (find-symbols \@ cnt))]

    (->> (into links mentions)
         (reduce (fn [acc* to]
                   (update-in acc* [to docname] (fnil conj #{}) path))
                 acc))))

(defn collect-links [ztx {{:keys [docname ann] :as meta} :zd/meta :as doc}]
  (let [acc (->> doc
                 (remove #(= "zd" (namespace (first %))))
                 (reduce (fn [acc [k cnt]]
                           (let [cnt-type (get-in ann [k :zd/content-type])]
                             (cond (= cnt-type :edn) (edn-links acc docname [k] cnt)
                                   (= cnt-type :zentext) (zentext-links acc docname [k] cnt)
                                   :else acc)))
                         {}))]
    (->> (:zd/subdocs doc)
         (reduce (fn [acc [k v]]
                   (let [sub-docname (symbol (str docname "." (name k)))]
                     (edn-links acc sub-docname [] v)))
                 acc))))

(defn patch-links [idx patch]
  (loop [acc idx
         [[to from :as links] & oth] (seq patch)]
    (if (nil? links)
      acc
      (let [new-acc (reduce (fn [acc* [from path]]
                              (update-in acc* [to from] (fnil into #{}) path))
                            (update acc to dissoc (ffirst from))
                            from)]
        (recur new-acc oth)))))

(defn *collect-macros [acc path node]
  (cond
    (and (list? node) (symbol? (first node)))
    (assoc acc path node)

    (map? node)
    (reduce (fn [acc [k v]]
              (*collect-macros acc (conj path k) v))
            acc
            node)

    ;; TODO add vector/seq traversal?
    :else acc))

(defn collect-macros [ztx {meta :zd/meta :as doc}]
  (->> doc
       (remove (fn [[k _]] (namespace k)))
       (filter (fn [[k _]] (= :edn (get-in meta [:ann k :zd/content-type]))))
       (reduce (fn [acc [k v]] (*collect-macros acc [k] v))
               {})))

(defn read-docs [ztx {:keys [root resource-path path content] :as doc}]
  (let [docname (meta/path->docname ztx resource-path)
        parts (str/split (str docname) #"\.")
        parent-link (->>  parts (butlast) (str/join "."))
        local-name (last parts)
        parent      (cond
                      (= (str docname) root) ""
                      (str/blank? parent-link) (and root (symbol root))
                      :else (symbol parent-link))
        doc-body {:zd/meta {:docname docname
                            :file resource-path
                            :ann {:parent {:zd/content-type :edn}}
                            ;; TODO add last updated from git to a document here?
                            :path path}
                  :parent parent
                  :zd/docname docname
                  :zd/name local-name
                  :zd/parent parent}
        doc (->> content (reader/parse ztx {}) (u/deep-merge doc-body))
        subdocs (->> (:zd/subdocs doc)
                     (mapv (fn [[k subdoc]]
                             (let [subdoc-name (symbol (str docname "." (name k)))]
                               (assoc subdoc :zd/meta
                                      {:docname subdoc-name
                                       :subdoc true
                                       :file resource-path
                                       :path path}
                                      ;; TODO: deprecated
                                      :parent docname
                                      :zd/docname subdoc-name
                                      :zd/name local-name
                                      :zd/parent docname)))))
        doc (assoc doc :zd/subs (into #{} (map :zd/docname subdocs)))]
    (into [doc] subdocs)))

(defn *remove-links [zrefs docname]
  (->> zrefs
       (reduce (fn [acc [k lnks]]
                 (if (contains? lnks docname)
                   (assoc acc k (dissoc lnks docname))
                   acc))
               zrefs)))

(*remove-links {'a {'b {}}} 'b)

(defn remove-links [ztx docname]
  (swap! ztx update :zrefs (fn [zrefs] (*remove-links zrefs docname))))

(defn put-doc [ztx {docname :zd/docname :as doc}]
  (let [links (collect-links ztx doc)
        macros (collect-macros ztx doc)]
    (swap! ztx assoc-in [:zdb docname] doc)
    (swap! ztx update :zrefs patch-links links)
    (swap! ztx update :zd/keys (fnil into #{}) (keys doc))
    (swap! ztx assoc-in [:zd/macros docname] macros)))

(defn is? [x c]
  (if (set? x) (contains? x c) (= x c)))

;; TODO: render infered attrs in a specific way
(defn infere [ztx docname {tp :zd/type p :zd/parent :as doc}]
  (let [doc (if (and (not tp) (is? (:zd/type (get-doc ztx p)) 'zd.class))
              (do
                (println :add docname :zd/type p)
                (assoc doc :zd/type p))
              doc)]
    (zen/pub ztx 'zd.events/on-doc-load doc)
    doc))

(defn infere-doc [ztx docname]
  (let [doc (get-doc ztx docname)
        idoc (infere ztx docname doc)]
    (swap! ztx assoc-in [:zdb docname] idoc)
    idoc))

(defn inference [ztx]
  (swap!
   ztx update :zdb
   (fn [zdb]
     (->> zdb
          (reduce (fn [acc [k v]]
                    (assoc acc k (infere ztx k v)))
                  {})))))

;; OBSOLETE
(defn load-document! [ztx {:keys [root resource-path path content] :as doc}]
  (let [docname (meta/path->docname ztx resource-path)
        parent-link (->> (str/split (str docname) #"\.") (butlast) (str/join "."))
        doc-body {:zd/meta {:docname docname
                            :file resource-path
                            :ann {:parent {:zd/content-type :edn}}
                            ;; TODO add last updated from git to a document here?
                            :path path}
                  :parent
                  (cond
                    (= (str docname) root) ""
                    (str/blank? parent-link) (symbol root)
                    :else (symbol parent-link))}
        doc (->> content
                 (reader/parse ztx {})
                 (u/deep-merge doc-body)
                 (meta/append-meta ztx))
        links (collect-links ztx doc)
        macros (collect-macros ztx doc)]
    ;; TODO move backlinks to toplevel and use merge
    (swap! ztx assoc-in [:zdb docname] doc)
    (swap! ztx update :zrefs patch-links links)
    (swap! ztx update :zd/keys (fnil into #{}) (keys doc))
    (swap! ztx assoc-in [:zd/macros docname] macros)
    (zen/pub ztx 'zd.events/on-doc-load doc)))

(defn load-links!
  "add backlinks to docs"
  [ztx]
  (loop [[[docname links :as i] & oth] (:zrefs @ztx)]
    (cond
      (nil? i) 'ok

      ;; TODO hanging link is found, add to idx and display error
      (nil? (get-in @ztx [:zdb docname]))
      (recur oth)

      :else
      (let [links* (mapcat (fn [[from paths]]
                             (for [p paths]
                               ;; TODO rename :doc to :from
                               {:to docname :path p :doc from}))
                           links)]
        (swap! ztx assoc-in [:zdb docname :zd/meta :backlinks] (set links*))
        (recur oth)))))

(defn eval-macros! [ztx]
  (doseq [[sym macros] (get @ztx :zd/macros)]
    (doseq [[docpath macro] macros]
      (swap! ztx update-in [:zdb sym]
             (fn [doc]
               (-> doc
                   (assoc-in [:zd/meta :ann (first docpath) :zd/macro] macro)
                   (assoc (first docpath) (methods/eval-macro! ztx doc docpath macro))))))))
