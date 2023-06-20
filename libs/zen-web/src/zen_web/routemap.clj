(ns zen-web.routemap
  (:require
   [clojure.string :as str]
   [zen.core :as zen]))

(defn pathify [path]
  (filterv #(not (str/blank? %)) (str/split path #"/")))

(defn is-glob? [k] (str/ends-with? (name k) "*"))

(defn- get-params [node]
  (when (map? node)
    (filter (fn [[k v]] (vector? k)) node)))

(defn- get-param [node]
  (first (filter (fn [[k v]] (vector? k)) node)))

(defn fn-param? [k]
  (and (vector? k)
       (let [f (first k)]
         (and (not (keyword? f)) (fn? f) ))))

(defn match-fn-params [node x]
  (when (map? node)
    (->> node
         (filter (fn [[k v]] (fn-param? k)))
         (reduce (fn  [acc [k v]]
                   (if-let [params ((first k) x)]
                     (conj acc [params v])
                     acc))
                 [])
         first)))

(defn regexp?
  [x]
  (instance? java.util.regex.Pattern x))

(defn *match [ztx
              {node-mws :mw :as node}
              [x & rpath :as path]
              {params :params mws :middlewares :as ctx}]
  (let [apis-mws (->> (:apis node)
                      (map #(zen/get-symbol ztx %))
                      (map :mw)
                      (apply concat))
        ctx (cond-> ctx
              node-mws (update :middlewares
                               (fnil into [])
                               (concat node-mws apis-mws)))
        next-node (get node x)]
    (cond (and (empty? path)
               (symbol? node))
          (assoc ctx :op node)

          next-node
          (*match ztx next-node rpath
                  (-> ctx
                   ;; TODO refactor conj
                      (update :path (fn [p] (conj (or p []) x)))
                      (update :resolution-path (fn [p] (conj (or p []) x)))))

          :else
          (let [params-match
                (->> (get-params node)
                     (map (fn [[[k] next-node]]
                            (*match ztx next-node rpath
                                    (-> ctx
                                        (assoc-in [:params k] x)
                                        (update :path (fn [p] (conj (or p []) k)))
                                        (update :resolution-path (fn [p] (conj (or p []) [k])))))))
                     (filter identity)
                     (first))

                apis-match
                (when-let [apis (:apis node)]
                  (->> apis
                       (map (fn [api-name]
                              (if-let [api (zen/get-symbol ztx api-name)]
                                ;; a bit of black magic to avoid transitive dep
                                ((ns-resolve (find-ns 'zen-web.core) 'resolve-route) ztx api path ctx)
                                (do
                                  (zen/error ztx 'zen-web/api-not-found {:api api-name})
                                  nil))))
                       (filter identity)
                       (first)))

                cur-idx (count (:resolution-path ctx))]

            (cond (string? (get (:resolution-path apis-match) (+ 1 cur-idx)))
                  apis-match

                  (and (nil? params-match) apis-match)
                  apis-match

                  (not (nil? params-match))
                  params-match

                  (:* node)
                  (*match ztx (:* node) (take-last 1 path)
                          (-> ctx
                              (assoc-in [:params :*] (vec (butlast path)))
                              (update :path (fn [p] (into p (butlast path))))
                              (update :resolution-path (fn [p] (conj p :*))))))))))

(defn resolve-route
  [ztx cfg path ctx]
  (*match ztx cfg path (-> ctx (update :resolution-path (fn [p] (conj (or p []) (:zen/name cfg)))))))

(defn routes [ztx cfg ctx]
  (let [ctx (cond-> ctx (:mw cfg)
                    (update :middlewares into (:mw cfg)))]
    (->> cfg
         (reduce (fn [acc [k v]]
                   (cond
                     (contains? #{:GET :POST :PUT :DELETE :OPTION :PATCH} k)
                     (conj acc (-> ctx
                                   (update :path conj k)
                                   (update :by conj k)
                                   (assoc :op v)))

                     (string? k)
                     (into acc (routes ztx v (-> (update ctx :path conj k)
                                                 (update :by conj k))))
                     (vector? k)
                     (into acc (routes ztx v (-> ctx
                                                 (update :path conj (first k))
                                                 (update :params conj (first k))
                                                 (update :by conj k))))
                     (= :apis k)
                     (->> v
                          (reduce (fn [acc api-name]
                                    (if-let [api (zen/get-symbol ztx api-name)]
                                      ;; a bit of black magic to avoid transitive dep
                                      (into acc ((ns-resolve (find-ns 'zen-web.core) 'routes) ztx api ctx))
                                      acc))
                                  acc))
                     :else acc))
                 []))))

(defn *routes [ztx cfg ctx]
  (routes ztx cfg (update ctx :by conj (:zen/name cfg))))
