(ns zd.fs
  (:require
   [zd.datalog :as d]
   [zd.meta :as meta]
   [zd.memstore :as memstore]
   [clojure.string :as str]
   [zd.gitsync :as gitsync]
   [zd.utils :as utils :refer [safecall]]
   [zd.fs.utils :as futils]
   [clojure.java.io :as io]
   [zen.core :as zen]))

(defn get-gistate [ztx]
  (->> [:zen/state :zd.fs :state :remote :gistate]
       (get-in @ztx)))

(defn get-state [ztx]
  (->> [:zen/state :zd.fs :state]
       (get-in @ztx)))

(defn name-to-dir [pths docname]
  (->> (str/split docname #"\.")
       butlast
       (str/join "/")
       (str (first pths) "/")))

(defn load-docs! [ztx root dirs]
  (println :load-docs)
  (->> (memstore/read-docs ztx {:path "~" :resource-path "zd.zd" :content (slurp (io/resource "zd.zd"))})
       (mapv (fn [doc] (memstore/put-doc ztx (assoc doc :zd/readonly true)))))
  (doseq [dir dirs]
    (let [dir (io/file dir)
          dir-path (.getPath dir)]
      ;; TODO: if we go with zd/type zd.class how do we find all classes before loading other documents?
      ;; TODO: load zd self schemas
      ;; load metadata
      (doseq [f (->> (file-seq dir)
                     (filter (fn [f] (str/includes? (.getName f) "_schema.zd"))))]
        (let [content (slurp f)]
          (meta/load-meta! ztx {:path (.getPath f)
                                :resource-path (subs (.getPath f) (inc (count dir-path)))
                                :content content})))
      ;; load documents
      (doseq [[path f] (->> (file-seq dir)
                            (map (fn [d] [(.getPath d) d]))
                            (sort-by first))]
        (when (and (str/ends-with? path ".zd")
                   (not (str/starts-with? (.getName f) ".")))
          (let [resource-path (subs path (inc (count dir-path)))
                content (slurp f)]
            (->> (memstore/read-docs ztx {:path path :root root :resource-path resource-path :content content})
                 (mapv (fn [doc] (memstore/put-doc ztx doc {:dont-validate true})))))))))
  (memstore/inference ztx))

(defonce queue (agent nil))

(defonce syncer (agent nil))

(defn reload [ztx root paths]
  (zen/pub ztx 'zd.events/on-load-start {})
  (swap! ztx dissoc :zdb :zd/schema :zrefs :zd/macros)
  (load-docs! ztx root paths)
  ;; (memstore/load-links! ztx)
  (memstore/eval-macros! ztx)
  (zen/pub ztx 'zd.events/on-load-complete {})
  ;; TODO think about return value
  'ok)

(defn fs-delete [ztx {:keys [filepath docname]}]
  (let [{r :root pths :paths} (get-state ztx)
        docname-sym (symbol docname)]
    (swap! ztx update :zdb dissoc docname-sym)
    ;; TODO: cleanup links and clear datalog
    (io/delete-file filepath)
    (when-let [repo (get-gistate ztx)]
      (gitsync/delete-doc ztx repo {:docpath filepath :docname docname}))
    (reload ztx r pths)))



;; TODO: obsolete
(defmethod zen/op 'zd.events/fs-delete
  [ztx config {_ev :ev {docname :docname} :params} & [_session]]
  (println :zd.fs/delete docname)
  (let [{r :root pths :paths} (get-state ztx)
        parts (str/split docname #"\.")
        filepath ;; TODO scan all paths?
        (str (first pths)
             "/"
             (str/join "/" parts)
             ".zd")]
    (send-off queue (fs-delete ztx {:filepath filepath :docname docname}))
    ;; TODO remove await
    (await queue)))


(defn fs-save [ztx {dn :docname cnt :content :keys [rename-to]} {:keys [resource-path dirname filepath prev-filepath]}]
  (let [{r :root pths :paths :as st} (get-state ztx)
        fs-save* (fn [ag]
                   (let [old-doc (memstore/get-doc ztx dn)])
                   (.mkdirs (io/file dirname))
                   (spit filepath cnt)
                   (when (symbol? rename-to)
                     ((fs-delete ztx {:filepath prev-filepath :docname dn}) ag)
                     (d/evict ztx (str dn)))
                   (if (str/includes? filepath "_schema")
                     (reload ztx r pths)
                     (->> (memstore/read-docs ztx {:path filepath
                                                   :root r
                                                   :resource-path resource-path
                                                   :content cnt})
                          (mapv (fn [doc]
                                  (memstore/put-doc ztx doc)
                                  (memstore/infere-doc ztx (:zd/docname doc))))))
                   'ok)]
    (utils/safecall ztx fs-save* {:type :zd.fs/save-error})))

(defn fs-commit [ztx {:keys [docname]} {:keys [filepath]}]
  (let [fs-commit* (fn [ag]
                     (when-let [repo (get-gistate ztx)]
                       (gitsync/commit-doc ztx repo {:docpath filepath :docname docname}))
                     (memstore/eval-macros! ztx)
                     'ok)]
    (utils/safecall ztx fs-commit* {:type :zd.fs/reload-error})))


;; TODO: obsolete
(defmethod zen/op 'zd.events/fs-save
  [ztx config {_ev :ev {dn :docname rename :rename-to :as ev} :params} & [_session]]
  (zen/pub ztx 'zd.events/fs-save {:docname dn :rename rename})
  (let [{pths :paths :as st} (get-state ztx)
        docname (if (symbol? rename)
                  (str rename)
                  (str dn))
        ;; TODO scan multiple file paths
        arg
        {:filepath (str (first pths) "/" (futils/docpath docname))
         :dirname (->> (str/split docname #"\.")
                       butlast
                       (str/join "/")
                       (str (first pths) "/"))
         ;; TODO check if resource path is needed
         :resource-path (futils/docpath docname)
         :prev-filepath (when (symbol? rename)
                          (str (first pths) "/" (futils/docpath dn)))}]
    (send-off queue (fs-save ztx ev arg))
    ;; TODO remove await
    (await queue)
    (send-off queue (fs-commit ztx ev arg))))

(defmethod zen/start 'zd.engines/fs
  [ztx {zd-config :zendoc :as config} & args]
  ;; TODO impl graceful shutdown if start is not possible
  (let [{:keys [remote root paths pull-rate]} (zen/get-symbol ztx zd-config)
        {repo :repo :as gistate}
        (-> ((utils/safecall ztx gitsync/init-remote {:type :gitsync/remote-init-error}) ztx remote)
            (:result))
        reload-fn*
        (fn [ag]
          (zen/pub ztx 'zd.events/on-pull-remote {:rate pull-rate})
          (let [sync-fn* (utils/safecall ztx gitsync/sync-remote {:type :gitsync/pull-remote-error})
                {st :status} (-> (sync-fn* ztx gistate) (:result))]
            (when (= :updated st)
              (reload ztx root paths))
            'reload-complete))
        reload-fn (utils/safecall ztx reload-fn* {:type :gitsync/pull-remote-error})]
    (reload ztx root paths)
    (if (instance? org.eclipse.jgit.api.Git repo)
      (letfn [(sync-fn [ag]
                (when-let [q (:ag (get-state ztx))]
                  (Thread/sleep pull-rate)
                  ;; TODO also check pull remote error
                  (send-off q reload-fn)
                  #_(when-not (= 'reload-complete (:result @q))
                    (send-off q reload-fn))
                  (send-off syncer sync-fn))
                'ok)]
        (send-off syncer sync-fn)
        {:ag queue
         :paths paths
         :root root
         :remote (assoc remote :gistate gistate)})
      ;; TODO if no git repo schedule init retry
      {:ag queue
       :root root
       :paths paths})))

(defmethod zen/stop 'zd.engines/fs
  [ztx config {r :remote :as state} & args]
  ;; TODO think about using shutdown agents
  (swap! ztx dissoc :zdb :zd/meta :zrefs :zd/macros))

(comment
  @queue

  @syncer

  (agent-error syncer)

  (agent-error queue)
  (agent-error syncer)

  (restart-agent syncer nil)
  (restart-agent queue nil)

  (add-watch queue :mywatcher (fn [key atom old-state new-state]
                                (println 'new-state old-state new-state))))
