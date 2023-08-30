(ns zd.git
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [stylo.core :refer [c]]
            [zd.methods :as methods])
  (:import [java.lang ProcessBuilder]
           [java.util.concurrent TimeUnit]))

(defn read-env  [override]
  (->
   (->> (System/getenv)
        (reduce (fn [acc [k v]]
                  (assoc acc (keyword k) v)) {}))
   (merge override)))

(defn read-stream [s]
  (let [r (io/reader s)]
    (loop [acc []]
      (if-let [l (.readLine r)]
        (recur (conj acc l))
        acc))))

(defn proc [{dir :dir env :env args :exec}]
  (let [proc (ProcessBuilder. (into-array String args))
        _ (when dir (.directory proc (io/file dir)))
        _ (when env
            (let [e (.environment proc)]
              #_(.clear e)
              (doseq [[k v] env]
                (.put e (name k) (str v)))))]
    proc))

(defn exec [{dir :dir env :env args :exec :as opts}]
  (let [prc (proc opts)
        p (.start prc)
        inp (.getInputStream p)
        err (.getErrorStream p)]
    (loop [stdout []
           stderr []]
      (if (.waitFor p 30 TimeUnit/SECONDS)
        {:status (.exitValue p)
         :stdout (into stdout (read-stream inp))
         :stderr (into stderr (read-stream err))}
        (recur (into stdout (read-stream inp))
               (into stderr (read-stream err)))))))

(defn run [opts]
  (let [prc (proc opts)]
    (.start prc)))

(defn init-env [{gh-key :ZO_GH_KEY dir :ZO_DIR repo :ZO_REPO :as opts}]
  {:GIT_SSH_COMMAND (format "ssh -i %s -o IdentitiesOnly=yes -o StrictHostKeyChecking=no" gh-key)})

(defn init-repo [{app :ZO_APP gh-key :ZO_GH_KEY dir :ZO_DIR repo :ZO_REPO :as opts}]
  (assert dir "ZO_DIR")
  (assert repo "ZO_REPO")
  (assert gh-key "ZO_GH_KEY")
  (assert app "ZO_APP")
  (assert (.exists (io/file gh-key)))
  (let [env (init-env opts)
        _ (println :env env)
        res (exec {:env env
                   :exec ["git" "clone" repo dir]
                   :dir "/tmp"})]
    (println res)))

(defn current-commit [{dir :ZO_DIR :as opts}]
  (let [env (init-env opts)]
    (-> (exec {:exec ["git" "rev-parse" "HEAD"] :env env :dir dir})
        (get-in [:stdout 0]))))

(defn start-process [opts]
  (println "START PROCESSS!" (str/split (:ZO_APP opts) #"\s+"))
  (let [dir (:ZO_DIR opts)
        env (init-env opts)
        args (str/split (:ZO_APP opts) #"\s+")]
    (exec {:exec ["git" "submodule" "init"] :env env :dir dir})
    (exec {:exec ["git" "submodule" "update"] :env env :dir dir})
    (let [prc (proc {:exec args :dir dir})]
      (.inheritIO prc)
      (.start prc))))

(defn start [{port :PORT gh-key :ZO_GH_KEY dir :ZO_DIR repo :ZO_REPO timeout :ZO_TIMEOUT :as opts}]
  (let [timeout (if timeout (Integer/parseInt timeout) 10000)
        dir (io/file dir)
        env (init-env opts)]
    (println (exec {:exec ["cp" gh-key "/tmp/gh-key"]}))
    (println (exec {:exec ["chmod" "400" "/tmp/gh-key"]}))
    (println (exec {:exec ["ls" "-lah" "/tmp/gh-key"]}))
    (let [opts (assoc opts :ZO_GH_KEY "/tmp/gh-key")
          env (init-env opts)]
      (when-not (.exists dir) (init-repo opts))
      (prn "Repo inited.")
      (loop [version (current-commit opts)
             p (start-process opts)]
        (exec {:exec ["git" "pull" "--rebase"] :env env :dir dir})
        (print "*") (flush)
        (let [test-version (current-commit opts)]
          (if (= test-version version)
            (do
              (Thread/sleep timeout)
              (recur version p))
            (do
              (println :restart-app test-version)
              (println "DESTROY" (.destroy p))
              (let [p (start-process opts)]
                (println :start p)
                (Thread/sleep timeout)
                (recur test-version p)))))))))

;; TODO try to re impl using java git

(defn create-timeline-data
  [[[_ author date] comment files]]
  (let [author (or author "")
        comment (or comment "")
        files (or files "")
        date (or date "")]
    {:comment (-> comment
                  first
                  clojure.string/trim)

     :user (-> author
               (clojure.string/split #"\s")
               second)

     :email (-> author
                (clojure.string/split #"\s")
                last
                rest
                butlast
                (clojure.string/join))

     :time (-> date
               (clojure.string/split #"\s")
               rest
               (->> (interpose " ")
                    (apply str)
                    (clojure.string/trim)))

     :files (vec files)}))

(defn get-history
  []
  (->> (exec {:exec ["git" "log"
                            "--name-only"
                            "--date=format-local:%Y-%m-%d %H:%M"
                            "--no-merges"
                            "-n" "30"]})
       :stdout
       (partition-by empty?)
       (remove (fn [x] (-> x first empty?)))
       (partition 3)
       (mapv create-timeline-data)
       (group-by (fn [l] (first (str/split (:time l) #"\s" 2))))))

(defn gh-index [ztx]
  (->> (map second (:zdb @ztx))
       (filter (fn [x]
                 (str/starts-with? (name (get-in x [:zd/meta :docname])) "people.")))
       (reduce (fn [acc {ghn :git/names gh :github :as res}]
                 (if (or gh ghn)
                   (->> ghn
                        (reduce (fn [acc nm]
                                  (assoc acc nm res))
                                (assoc acc gh res)))
                   acc)))))

(defn dir [ztx] (:zd/dir @ztx))

(defn init [ztx]
  (exec {:dir (dir ztx) :exec ["git" "init"]}))


(defn commit [ztx message]
  (exec {:dir (dir ztx) :exec ["git" "add" "."]})
  (exec {:dir (dir ztx) :exec ["git" "commit" "-m" message]}))

(defn history [ztx & [limit]]
  (->> (exec {:dir (dir ztx)
              :exec ["git" "log"
                     "--name-only"
                     "--date=format-local:%Y-%m-%d %H:%M"
                     "--no-merges"
                     "-n" (str (or limit 30))
                     ]})
       :stdout
       (partition-by empty?)
       (remove (fn [x] (-> x first empty?)))
       (partition 3)
       (mapv create-timeline-data)
       (group-by (fn [l] (first (str/split (:time l) #"\s" 2))))
       (sort-by first)
       (reverse)
       (mapv (fn [[date commits]] {:date date :commits commits}))))

(defn changes [ztx]
  (->> (exec {:dir (dir ztx)
              :exec ["git" "status" "-s"]})
       :stdout
       (mapv (fn [s]
               (let [[change file] (str/split (str/trim s) #"\s+" 2)]
                 {:change (get {"??" :new "M" :modified "D" :deleted} change) :file file})))))


(defn pull [ztx])


;; (defn gh-user [ztx gh-idx l]
;;   (if-let [u (get gh-idx (when-let [un (:user l)] (str/trim un)))]
;;     (link/symbol-link ztx (:zd/name u))
;;     [:b (or (:user l) (:email l))]))

;; (defmethod methods/renderkey :git/timeline
;;   [ztx {ps :paths :as ctx} block]
;;   (let [gh-idx (gh-index ztx)]
;;     [:div
;;      ;; TODO show file deletion separately
;;      (for [[date ls] (->> (get-history)
;;                           (sort-by first)
;;                           (reverse))]
;;        [:div
;;         [:div {:class (c :border-b :font-bold [:mt 2])} date]
;;         (for [l (reverse (sort-by :time ls))]
;;           [:div
;;            [:div {:class (c :flex :items-baseline [:space-x 2] [:ml 0] [:py 1])}
;;             [:div {:class (c [:text :gray-600])} (last (str/split (:time l) #"\s" 2))]
;;             [:div (gh-user ztx gh-idx l)]
;;             [:div (:comment l)]]
;;            [:ul {:class (c [:ml 6])}
;;             (->> (:files l)
;;                  (filter (fn [x]
;;                            (some #(str/starts-with? x %) ps)))
;;                  (map (fn [x]
;;                         (let [docname (->> ps
;;                                            (map (fn [p]
;;                                                   (let [to-replace
;;                                                         (if (str/ends-with? p "/")
;;                                                           p
;;                                                           (str p "/"))]
;;                                                     (str/replace x to-replace ""))))
;;                                            (filter #(not= % x))
;;                                            (first))]
;;                           (symbol (-> docname
;;                                       (str/replace "/" ".")
;;                                       (str/replace ".zd" ""))))))
;;                  (sort)
;;                  (mapv (fn [x] [:li (link/symbol-link ztx x)]))
;;                  (apply conj [:div]))]])])]))
