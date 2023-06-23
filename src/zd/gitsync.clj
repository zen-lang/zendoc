(ns zd.gitsync
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clj-jgit.porcelain :as git]))

(defn commit-doc [ztx repo {p :docpath d :docname}]
  (let [;; TODO sync all untracked docs at gitsync start?
        {:keys [untracked modified] :as status} (git/git-status repo)
        git-config (git/git-config-load repo)]
    (doseq [m (into untracked modified)]
      (when (str/includes? p m)
        (let [uname (or (.getString git-config "user" nil "name") "unknown editor")
              email (or (.getString git-config "user" nil "email") "unknown-editor@example.com")]
          (git/git-add repo m)
          (let [msg (if (contains? untracked m)
                      (str "Create " d)
                      (str "Edit " d))]
            (git/git-commit repo msg :committer {:name uname :email email})))))))

(defn delete-doc [ztx repo {p :docpath d :docname}]
  (let [{:keys [missing]} (git/git-status repo)
        git-config (git/git-config-load repo)
        uname (or (.getString git-config "user" nil "name") "unknown editor")
        email (or (.getString git-config "user" nil "email") "unknown-editor@example.com")]
    (doseq [m missing]
      (when (str/includes? p m)
        (git/git-rm repo m)
        (git/git-commit repo (str "Delete " d) :committer {:name uname :email email})))))

(defn sync-remote [ztx repo]
  (let [pull-result (git/git-pull repo)]
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
          {:status :updated})))))

(defn init-remote [ztx {:keys [from branch to] :as remote}]
  (when (string? to)
    (let [pulled? (.exists (io/file to))
          repo (if pulled?
                 (git/load-repo to)
                 (git/git-clone from :dir to))]
      ;; TODO add create/checkout default branch if necessary?
      (when branch
        (git/git-checkout repo branch))
      (git/git-pull repo)
      #_(when-not pulled?
          (git/git-submodule-init repo))
      #_(git/git-submodule-update repo :strategy :recursive)
      repo)))
