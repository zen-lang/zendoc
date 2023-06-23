(ns zd.fs
  (:require
   [zd.meta :as meta]
   [zd.memstore :as memstore]
   [clojure.string :as str]
   [zd.gitsync :as gitsync]
   [zd.utils :as utils]
   [clojure.java.io :as io]
   [zen.core :as zen]))

(defn get-repo [ztx]
  (->> [:zen/state :zd.fs :state :remote :repo]
       (get-in @ztx)))

(defn get-state [ztx]
  (->> [:zen/state :zd.fs :state]
       (get-in @ztx)))

(defn load-docs! [ztx root dirs]
  (doseq [dir dirs]
    (let [dir (io/file dir)
          dir-path (.getPath dir)]
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
            (memstore/load-document! ztx {:path path
                                          :root root
                                          :resource-path resource-path
                                          :content content})))))))

(defonce queue (agent nil))

(defonce syncer (agent nil))

(defn reload [ztx root paths]
  (println :zd.fs/reload)
  (swap! ztx dissoc :zdb :zd/schema :zrefs :zd/macros)
  (load-docs! ztx root paths)
  (memstore/load-links! ztx)
  (memstore/eval-macros! ztx)
  ;; TODO think about return value
  'ok)

(defmethod zen/op 'zd.events/fs-delete
  [ztx config {_ev :ev {docname :docname} :params} & [_session]]
  (println :zd.fs/delete docname)
  (let [{r :root pths :paths} (get-state ztx)
        parts (str/split docname #"\.")
        filepath ;; TODO scan all paths?
        (str (first pths)
             "/"
             (str/join "/" parts)
             ".zd")
        fs-delete (utils/safecall (fn [ag]
                                    (io/delete-file filepath)
                                    (when-let [repo (get-repo ztx)]
                                      (gitsync/delete-doc ztx repo {:docpath filepath :docname docname}))
                                    ;; TODO implement deletion of a single document
                                    (reload ztx r pths))
                                  {:type 'zd.fs/delete-doc-error})]
    (send-off queue fs-delete)
    (await queue)))

(defmethod zen/op 'zd.events/fs-save
  [ztx config {_ev :ev {docname :docname cnt :content} :params} & [_session]]
  ;; TODO emit zen event
  (println :zd.fs/save docname)
  (let [{r :root pths :paths} (get-state ztx)
        dirname
        (->> (str/split docname #"\.")
             butlast
             (str/join "/")
             (str (first pths) "/"))
        docpath (str (str/replace docname "." "/") ".zd")
        filepath (str (first pths) "/" docpath)
        fs-save (utils/safecall (fn [ag]
                                  (.mkdirs (io/file dirname))
                                  (spit filepath cnt)
                                  (if (str/includes? docname "_schema")
                                    (reload ztx r pths)
                                    (memstore/load-document! ztx {:path filepath
                                                                  :root r
                                                                  :resource-path docpath
                                                                  :content cnt}))
                                  'ok)
                                {:type :zd.fs/save-error})
        fs-reload (utils/safecall (fn [ag]
                                    (when-let [repo (get-repo ztx)]
                                      (gitsync/commit-doc ztx repo {:docpath filepath :docname docname}))
                                    (memstore/load-links! ztx)
                                    (memstore/eval-macros! ztx)
                                    'ok)
                                  {:type :zd.fs/reload-error})]
    (send-off queue fs-save)
    (await queue)
    (send-off queue fs-reload)))

(defmethod zen/start 'zd.engines/fs
  [ztx {zd-config :zendoc :as config} & args]
  ;; TODO impl graceful shutdown if start is not possible
  (let [{:keys [remote root paths pull-rate]} (zen/get-symbol ztx zd-config)
        repo
        (-> ((utils/safecall gitsync/init-remote {:type :gitsync/remote-init-error}) ztx remote)
            (:result))
        reload-fn
        (utils/safecall (fn [ag]
                          (let [{st :status}
                                (-> ((utils/safecall gitsync/sync-remote {:type :gitsync/pull-remote-error}) ztx repo)
                                    (:result))]
                            (when (= :updated st)
                              (reload ztx root paths))
                            'ok))
                        {:type :gitsync/pull-remote-error})]
    (reload ztx root paths)
    (if (instance? org.eclipse.jgit.api.Git repo)
      (letfn [(sync-fn [ag]
                (when-let [q (:ag (get-state ztx))]
                  (Thread/sleep pull-rate)
                  (send-off q reload-fn)
                  (send-off syncer sync-fn))
                'ok)]
        (send-off syncer sync-fn)
        {:ag queue
         :paths paths
         :root root
         :remote (assoc remote :repo repo)})
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

  (agent-error queue)
  (agent-error syncer)

  (restart-agent syncer nil)

  (add-watch queue :mywatcher (fn [key atom old-state new-state]
                                (println 'new-state old-state new-state))))
