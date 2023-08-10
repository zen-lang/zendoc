(ns zd.parser
  (:require
   [clojure.string :as str]
   [edamame.core :as reader]
   [clojure.walk]
   [clojure.java.io :as io])
  (:import [java.io StringReader]))

(defn is-comment? [l]
  (str/starts-with? l ";"))

(defn is-ann? [l]
  (str/starts-with? l "^"))

(defn is-key? [l]
  (str/starts-with? l ":"))

(defn is-block? [l]
  (or (is-ann? l) (is-key? l)))

(defn btype [l]
  (cond
    (is-ann? l) :ann
    (is-key? l) :key))

(defmulti parse! (fn [ztx ctx l acc] (btype (or l ""))))

(defmulti collect! (fn [ztx ctx l acc] (btype (or l ""))))


(defn split [pred coll & filters]
  (loop [left [] [n & coll*] coll]
    (cond (nil? n) (list (seq left) nil)
          (some #(% n) filters) (recur left coll*)
          (pred n) (list (seq left) (conj coll* n))
          :else (recur (conj left n) coll*))))

(defmethod collect! :ann
  [ztx doc l lines]
  (let [[_ head] (split is-key? lines)]
    (if (nil? head)
      ;; TODO add zd/error
      [head (rest lines)]
      (let [[cnt tail] (collect! ztx doc (first head) (rest head))]
       [(conj cnt (first head)) tail]))))


(defn saferead [v rootname]
  (try
    (->> 
     (reader/parse-string v)
     (clojure.walk/postwalk (fn [x] (if (= x '.) rootname x))))
    (catch Exception e
      (prn ":saferead parsing warning"))))

(defn collect-edn [ztx doc lines]
  (loop [[f & tail :as ls] lines
         acc []]
    (cond (nil? ls) [nil lines]
          (str/blank? f) (recur tail acc)
          (and (is-block? f) (empty? acc)) [nil ls]
          :else
          (if (try (->> (conj acc f)
                        (apply str)
                        (reader/parse-string))
                   (catch Exception _))
            [(seq (conj acc f)) tail]
            (recur tail (conj acc f))))))

(defmethod collect! :key
  [ztx doc l lines]
  (let [cnt-type (->> (str/trim l)
                      (reverse)
                      (rest)
                      (take-while #(not= % \space))
                      (reverse)
                      (apply str)
                      keyword)

        multiline-edn? (and (= \/ (last l))
                            (or (= :datalog cnt-type)
                                (= :edn cnt-type)))]
    (if multiline-edn?
      (let [[cnt tail] (collect-edn ztx doc lines)]
        ;; TODO add error to :zd/errors
        [cnt tail])
      (let [[cnt tail] (split is-block? lines is-comment?)]
        [cnt tail]))))

(defmethod parse! :ann
  [ztx ctx ann ls]
  (let [key (->> (first ls)
                 (take-while #(not= % \space))
                 (rest)
                 (apply str)
                 (keyword))
        [k val] (split #(= % \space) (rest ann))
        ann (keyword (apply str k))
        ctx (assoc-in ctx [:zd/annotations key ann] (saferead (apply str val) (:zd/rootname ctx)))]
    (parse! ztx ctx (first ls) (rest ls))))

(defmethod parse! :key
  [ztx ctx l ls]
  (let [[k val] (split #(= % \space) (str/trim l))
        multiline? (= \/ (last val))
        k (keyword (apply str (rest k)))
        cnt-type (if multiline?
                   (let [ann (str/trim (apply str (butlast val)))]
                     (if (str/blank? ann)
                       :zentext
                       (keyword ann)))
                   :edn)
        lines (if multiline?
                (str/join "\n" ls)
                (apply str val))
        ;; TODO add read multimethod ?
        cnt (cond (str/blank? lines) lines
                  (= cnt-type :edn) (saferead lines (:zd/rootname ctx))
                  (= cnt-type :datalog) (saferead lines (:zd/rootname ctx))
                  (= cnt-type :zentext) (str/join "\n" ls))
        ctx* (-> ctx
                 (update-in [:zd/view] conj [k (cond-> (merge {:type cnt-type } (get-in ctx [:zd/annotations k]))
                                                 multiline? (assoc :multiline true))])
                 (assoc k (or cnt lines)))]
    (cond-> ctx*
      (and (nil? cnt) (or (= cnt-type :edn) (= cnt-type :datalog)))
      (update-in [:zd/meta :errors] conj {:type :edn-reading-error
                                          :path [k]
                                          :message "Invalid edn format"}))))

(defmethod parse! :default
  [ztx ctx l lines]
  (update-in ctx [:zd/meta :errors] conj
             {:type :block-undefined
              :message "can not parse block"
              :lines (conj lines l)}))

(defn get-lines [s]
  (line-seq (io/reader (StringReader. s))))

(defn doc-name [l idx]
  (let [s (first (str/split (subs l 1) #"\s"))
        s (if (str/blank? s) (str "doc-" idx) s)]
    (symbol s)))

(defn parse* [ztx docname lines idx]
  (loop [ctx {:zd/view [] :zd/docname docname :zd/rootname docname}
         [l & ls] lines]
    (cond (nil? l)        ctx
          (str/blank? l)  (recur ctx ls)
          (is-comment? l) (recur ctx ls)
          (str/starts-with? l "&") (let [subdocname (doc-name l idx)]
                                     (recur (assoc ctx
                                                   :zd/docname (symbol (str docname "." subdocname))
                                                   :zd/parent docname
                                                   :zd/name (str subdocname)
                                                   :zd/subdoc? true) ls))
          (is-block? l)   (let [[acc ls] (collect! ztx ctx l ls)]
                            (recur (parse! ztx ctx l acc) ls))
          :else (recur ctx ls))))

(defn split-docs [lines]
  (loop [[l & ls] lines
         cur []
         acc []]
    (if (nil? l)
      (conj acc cur)
      (if (str/starts-with? l "&")
        (recur ls [l] (conj acc cur))
        (recur ls (conj cur l) acc)))))

(split-docs ["a" "b" "&c" "d" "&e" "f"])

;; split into documents
(defn parse [ztx docname text]
  (let [lines (get-lines text)
        docs (split-docs lines)]
    (->> docs
         (map-indexed (fn [idx lines] (-> (parse* ztx docname lines idx)
                                         (dissoc :zd/annotations :zd/rootname)))))))
