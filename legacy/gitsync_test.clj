(ns zd.gitsync-test
  (:require
   [zd.test-utils :as tutils]
   [zen-web.core :as web]
   [matcho.core :as matcho]
   [zen.core :as zen]
   [zd.fs :as fs]
   [clojure.test :refer [deftest is testing]]))

(defonce ztx (zen/new-context {}))

;; forces gitsync to work
;; look at the remote to compare and check commits order
(deftest ^:kaocha/pending gitsync

  (zen/stop-system ztx)

  (zen/read-ns ztx 'zd)

  (zen/read-ns ztx 'zd.demo)

  (zen/start-system ztx 'zd.demo/system)

  (def st (fs/get-state ztx))

  (is (nil? (agent-errors fs/queue)))

  ;; repo is ready
  (is (instance? org.eclipse.jgit.api.Git  (get-in st [:remote :gistate :repo])))

  (matcho/assert
   {:status 200}
   (web/handle ztx 'zd/api
               {:uri "/_draft/edit"
                :request-method :put
                :body (tutils/req-body ":zd/docname example\n:title \"my title\"\n:desc /")}))

  (matcho/assert
   {:status 200}
   (web/handle ztx 'zd/api
               {:uri "/example/edit"
                :request-method :put
                :body (tutils/req-body ":zd/docname example\n:title \"my title\"\n:desc /\na description")}))

  (matcho/assert
   {:status 200 :body "/index"}
   (web/handle ztx 'zd/api {:uri "/example" :request-method :delete}))

  (await fs/queue)

  (is (nil? (agent-errors fs/queue)))

  (zen/stop-system ztx))
