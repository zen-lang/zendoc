(ns zd.parser
  (:require
   [clojure.string :as str]
   [edamame.core :as reader]
   [clojure.walk]
   [zd.methods :as methods]
   [clojure.java.io :as io])
  (:import [java.io StringReader]))

(defn get-lines [s]
  (line-seq (io/reader (StringReader. s))))

(re-matches #":([^ ]*)\s+([^/]*)/(.*)$" ":key zen/ text")
(re-matches #":([^ ]*)\s+([^/]*)/(.*)$" ":key /")
(re-matches #":([^ ]*)\s+([^/]*)/(.*)$" ":key text")


(defn parse-annotation [s]
  (let [[nm s] (str/split (str/trim (subs s 1)) #"\s+" 2)
        v (if (re-matches #"^(\[|\{|\"|:).*" (or s ""))
            (try
              (edamame.core/parse-string s)
              (catch Exception e s))
            s)]
    [(keyword nm) v]))

(parse-annotation "^title Here is some title")
(parse-annotation "^title \"Here is some title\"")
(parse-annotation "^title {:a 1}")
(parse-annotation "^title [1 2 3]")

(defmethod methods/do-parse :unknown [_ _ s] s)
(defmethod methods/do-parse :str [_ _ s] (str/trim s))
(defmethod methods/do-parse :zentext [_ _ s] (str/trim s))
(defmethod methods/do-parse :? [_ _ s] (str/trim s))

(defmethod methods/do-parse
  :edn [{parent :parent} _ s]
  (try
    (->> (edamame.core/parse-string s)
         (clojure.walk/postwalk (fn [x] (if (= x '.) parent x))))
    (catch Exception e
      (str "Error(" (.getMessage e) ")" s))))

(defn parse-key-name [l]
  (if-let [[_ k tp] (re-matches #":([^ ]*)\s+([^/]*)/$" l)]
    [(keyword k) (if (= "" tp)  :zentext (keyword tp)) "" true]
    (if-let [[_ k s] (re-matches #":([^ ]*)\s+\|(.*)$" l)]
      [(keyword k) :str (str/trim s) false]
      (let [[k l] (str/split l #"\s" 2)]
        [(keyword (subs k 1)) :edn l false]))))

(defn parse-key [{anns :anns [l & ls] :key} docname parent]
  (let [[k tp l multiline] (parse-key-name l)
        annotations (->> anns
                         (reduce
                          (fn [acc ls]
                            (let [s (str/join "\n" ls)
                                  [nm v] (parse-annotation s)
                                  ann    (methods/annotation (keyword nm) v)]
                              (merge acc ann)))
                          (cond-> {:type tp} multiline (assoc :multiline true))))
        data  (if multiline (->> (into [l] ls) (str/join "\n")) l)
        value (methods/do-parse {:docname docname :parent parent}
                                (or (:type annotations) :unknown) data)]
    {:key k
     :value value
     :annotations annotations}))


(defn parse-doc-name [s docname i]
  (let [[_ nm _ tp] (re-matches #"^&([^ ]*)(\s+(.*))?$" s)
        tp (when tp (try (edamame.core/parse-string tp)
                         (catch Exception _
                           (println :error/parsing-subdoc s)
                           nil)))]
    [(symbol (str docname "." (if (str/blank? nm) (str "doc-" i) nm))) tp]))

(parse-doc-name "&name" 'doc 1)
(parse-doc-name "&name type" 'doc 1)
(parse-doc-name "&name #{a.b c.d}" 'doc 1)
(parse-doc-name "& type" 'doc 1)
(parse-doc-name "&" 'doc 1)
(parse-doc-name "& " 'doc 1)

(defn parse-doc [{nm :name ks :keys} rootname i]
  (let [[docname tp] (if nm (parse-doc-name nm rootname i) [rootname nil])]
    (->> ks
         (reduce (fn [acc k]
                   (let [{k :key ann :annotations v :value} (parse-key k docname rootname)]
                     (-> acc
                         (assoc k v)
                         (update :zd/view conj [k ann]))))
                 (cond-> {:zd/view []
                          :zd/docname docname}
                   nm (assoc :zd/subdoc? true :zd/parent rootname)
                   tp (assoc :zd/type tp :zd/view [[:zd/type {:type :edn}]]))))))

;; :start
;;   ^ -> annotation | :ann
;;   : -> key  | :key or :zentext
;;   other -> ignore | :start
;; :ann
;;   ^ -> new annotation | :ann
;;   : -> key | :key or zentext
;;   other -> conj to current annotation | :ann
;; :zentext
;;   ^ -> finalize key | :ann
;;   : -> finalize key | :key
;;   ``` ->  start | :block
;;   other -> add to key | :key
;; :block
;;   ``` -> finalize block
;;   other -> add to block | block
;; :key
;;   ^ -> finalize key | :ann
;;   : -> finalize key | :key
;;   other -> add to key | :key

(defn close-annotation [ctx]
  (if-let [ann  (seq (:ann ctx))]
    (-> ctx
        (update  :anns conj ann)
        (assoc :ann []))
    ctx))

(defn add-to-annotation [ctx l]
  (update ctx :ann conj l))

(defn close-key [ctx]
  (if (seq (:key ctx))
    (-> ctx
        (update :keys conj (select-keys ctx [:anns :key]))
        (assoc :key [] :ann [] :anns []))
    ctx))

(defn add-to-key [ctx l]
  (update ctx :key conj l))

(defn split-keys [lines docname]
  (loop [[l & ls :as pls] lines
         state :start
         ctx {:anns []
              :ann []
              :key []
              :keys []}]
    (if (nil? l)
      (cond-> (:keys ctx)
        (seq (:key ctx)) (conj (select-keys ctx [:anns :key])))
      (cond
        (= state :start)
        (cond
          (str/starts-with? l "^")
          (let [ctx (add-to-annotation ctx l)]
            (recur ls :ann ctx))

          (str/starts-with? l ":")
          (let [ctx (-> (close-annotation ctx)
                        (assoc :key [l]))]
            (if (str/ends-with? l "/")
              (let [state (if (str/ends-with? l " /") :zentext :key)]
                (recur ls state ctx))
              (let [ctx (close-key ctx)]
                (recur ls :start ctx))))

          (str/blank? l) (recur ls :start ctx)

          :else (do (println :parser/ignore docname (pr-str l))
                    (recur ls :start ctx)))

        (= state :ann)
        (cond
          (str/starts-with? l "^")
          (let [ctx (-> (close-annotation ctx)
                        (add-to-annotation l))]
            (recur ls :ann ctx))

          (str/starts-with? l ":")
          (let [ctx (close-annotation ctx)]
            (recur pls :start ctx))

          :else
          (let [ctx (add-to-annotation ctx l)]
            (recur ls state ctx)))

        (= state :key)
        (cond
          (or (str/starts-with? l "^") (str/starts-with? l ":"))
          (let [ctx (close-key ctx)]
            (recur pls :start ctx))

          :else
          (let [ctx (add-to-key ctx l)]
            (recur ls :key ctx)))

        (= state :zentext)
        (cond
          (or (str/starts-with? l "^") (str/starts-with? l ":"))
          (let [ctx (close-key ctx)]
            (recur pls :start ctx))

          (str/starts-with? l "```")
          (let [ctx (add-to-key ctx l)]
            (recur ls :block ctx))

          :else
          (let [ctx (add-to-key ctx l)]
            (recur ls :zentext ctx)))

        (= state :block)
        (cond
          (str/starts-with? l "```")
          (let [ctx (add-to-key ctx l)]
            (recur ls :zentext ctx))

          :else
          (let [ctx (add-to-key ctx l)]
            (recur ls :block ctx)))

        :else
        (do
          (println :parser/ignore docname l)
          (recur ls state ctx))))))

(defn split-docs [lines]
  (loop [[l & ls] lines
         cur {:lines []}
         acc []]
    (if (nil? l)
      (conj acc cur)
      (if (str/starts-with? l "&")
        (recur ls {:name l :lines []} (conj acc cur))
        (recur ls (update cur :lines conj l) acc)))))

(split-docs ["a" "b" "&c" "d" "&e" "f"])

;; split into documents
(defn parse [ztx docname text & [meta]]
  (let [lines (->> (get-lines text)
                   (remove #(str/starts-with? % ";;")))
        [doc & subdocs]  (->> (split-docs lines)
                              (map-indexed (fn [i doc]
                                             (-> doc
                                                 (dissoc :lines)
                                                 (assoc :keys (split-keys (:lines doc) docname))
                                                 (parse-doc docname i)
                                                 ;; TODO: handle parent
                                                 (merge  (dissoc meta :zd/parent))
                                                 (update :zd/parent (fn [x] (or x (:zd/parent meta))))))))]
    (cond-> doc (seq subdocs) (assoc :zd/subdocs (into [] subdocs)))))
