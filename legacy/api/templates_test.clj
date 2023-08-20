(ns zd.api.templates-test
  (:require
   [cheshire.core :as json]
   [clojure.string :as str]
   [hickory.select :as s]
   [hickory.core :as hickory]
   [zd.test-utils :as tutils]
   [zen-web.core :as web]
   [matcho.core :as matcho]
   [zd.api]
   [clojure.test :refer [deftest is testing]]
   [zen.core :as zen]))

(defonce ztx (zen/new-context {}))

(defn parse-template [editor-cfg]
  (let [parsed-config
        (->> (hickory/parse editor-cfg)
             (hickory/as-hickory)
             (s/select (s/id :editor-config))
             (first)
             :content
             (first))]
    (-> (str/replace parsed-config "var zendoc=" "")
        (json/parse-string true)
        (:text))))

(deftest people-template
  (tutils/prepare! ztx)

  (testing "people template is used"

    (def editor (:body (web/handle ztx 'zd/api {:uri "/people._draft/edit"})))

    (is (= ":zd/docname people._draft\n:title \"People template\"\n:tags #{}\n:role #{\"account manager\"}\n:desc /"
           (parse-template editor))))

  (testing "if template is not found nothing is prefilled"
    (def editor (:body (web/handle ztx 'zd/api {:uri "/clients._draft/edit"})))

    (is (= ":zd/docname clients._draft\n" (parse-template editor))))

  (testing "create root template to be used by default"
    (def doc ":zd/docname _template\n:title \"mytitle\"\n:desc \"\"\n:mycustomkey \"a string\"")

    (matcho/assert
     {:status 200}
     (web/handle ztx 'zd/api
                 {:uri "/_draft/edit"
                  :request-method :put
                  :body (tutils/req-body doc)}))

    (def editor (:body (web/handle ztx 'zd/api {:uri "/clients._draft/edit"})))
    (is (= ":zd/docname clients._draft\n:title \"mytitle\"\n:desc \"\"\n:mycustomkey \"a string\""
           (parse-template editor)))

    (matcho/assert
     {:status 200 :body string?}
     (web/handle ztx 'zd/api {:uri "/_template"
                              :request-method :delete}))

    (is (nil? (tutils/read-doc "_template.zd")))))
