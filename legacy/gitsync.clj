(ns zd.gitsync
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clj-jgit.porcelain :as git]))

;; TODO: may be use shell git integration

(defn commit-doc [ztx {:keys [repo ident]} {p :docpath d :docname}]
  (git/with-identity ident
    (let [;; TODO sync all untracked docs at gitsync start?
          {:keys [untracked modified] :as status} (git/git-status repo)
          git-config (git/git-config-load repo)]
      (doseq [m (into untracked modified)]
        (when (str/includes? p m)
          (let [uname (or (.getString git-config "user" nil "name") "unknown editor")
                email (or (.getString git-config "user" nil "email") "unknown-editor@zendoc.me")]
            (git/git-add repo m)
            (let [msg (if (contains? untracked m)
                        (str "Create " d)
                        (str "Edit " d))]
              (git/git-commit repo msg :committer {:name uname :email email}))))))))

(defn delete-doc [ztx {:keys [repo ident]} {p :docpath d :docname}]
  (git/with-identity ident
    (let [{:keys [missing]} (git/git-status repo)
          git-config (git/git-config-load repo)
          uname (or (.getString git-config "user" nil "name") "unknown editor")
          email (or (.getString git-config "user" nil "email") "unknown-editor@zendoc.me")]
      (doseq [m missing]
        (when (str/includes? p m)
          (git/git-rm repo m)
          (git/git-commit repo (str "Delete " d) :committer {:name uname :email email}))))))

(defn sync-remote [ztx {repo :repo ident :ident}]
  (git/with-identity ident
    (let [pull-result (git/git-pull repo {:ff-mode :ff
                                          :rebase-mode :none
                                          :strategy :ours})]
      ;; TODO resolve merge conflicts
      (when (.isSuccessful pull-result)
        (let [updated? (-> (.getFetchResult pull-result)
                           (.getTrackingRefUpdates)
                           (.isEmpty)
                           (not))]
          ;; TODO check index status?
          (git/git-push repo)
          (when updated?
            (println :zd.gitsync/sync-reload)
            {:status :updated}))))))

(defn init-remote [ztx {:keys [from branch keystore to] k :key :as remote}]
  (let [ident {:name (or k ["id_rsa" "id_dsa" "id_ecdsa" "id_ed25519"])
               ;; TODO support for other os
               :trust-all? true
               :key-dir (or keystore (str (System/getProperty "user.home") "/.ssh"))}]
    (when (string? to)
      (git/with-identity ident
        (let [pulled? (.exists (io/file to))
              repo (if pulled?
                     (git/load-repo to)
                     (git/git-clone from :dir to))]
          ;; TODO add checkout branch if necessary?
          (when branch
            (git/git-checkout repo branch))
          (git/git-pull repo)
          #_(when-not pulled?
              (git/git-submodule-init repo))
          #_(git/git-submodule-update repo :strategy :recursive)
          {:ident ident :repo repo})))))
