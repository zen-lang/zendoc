(ns zd.api.validation-test
  (:require
   [zd.test-utils :as tutils]
   [zen-web.core :as web]
   [matcho.core :as matcho]
   [zd.api]
   [clojure.test :refer [deftest is testing]]
   [zen.core :as zen]))

;; TODO add test when schema is embedded on the top level e.g. backlog._schema , backlog.features.my-feature

(defonce ztx (zen/new-context {}))

(deftest doc-validation
  "validation works for :schema, :key schema and &subdoc :schema"

  (tutils/prepare! ztx)

  (def invalid-doc ":zd/docname customers._draft\n:title #{mytitle}\n:icon \"a string\"")

  (def doc ":zd/docname customers.zero\n:title \"zero inc\"\n:rel #{rel.partner}\n:techs #{techs.clojure}\n:desc \"mydesc\" ")

  (def errs
    [{:type :docname-validation,
      :path [:zd/docname],
      :message "Rename :zd/docname from _draft"}
     ;; two keys defined in _schema
     {:type :doc-validation,
      :message "Expected type of 'string, got 'persistenthashset",
      :path [:title]}
     {:type :doc-validation,
      :message "Expected type of 'vector, got 'string",
      :path [:icon]}
     ;; required in customers._schema
     {:type :doc-validation, :message ":rel is required", :path [:rel]}
     ;; required in _schema
     {:type :doc-validation,
      :path [:desc]}])

  (testing ":zd/docname, keys from both _schema and customers._schema are validated"
    (matcho/assert
     {:status 422
      :body
      {:message "document validation failed"
       :docname "customers._draft"
       :errors errs}}
     (web/handle ztx 'zd/api
                 {:uri "/customers._draft/edit"
                  :request-method :put
                  :body (tutils/req-body invalid-doc)})))

  (testing "extra key :techs is allowed, required keys pass validation"
    (matcho/assert
     {:status 200}
     (web/handle ztx 'zd/api
                 {:uri "/customers._draft/edit"
                  :request-method :put
                  :body (tutils/req-body doc)}))

    (is (tutils/read-doc "customers/zero.zd"))

    (is (= 200 (:status (web/handle ztx 'zd/api {:uri "/customers.zero"
                                                 :request-method :delete})))))

  (testing "keys in subdocuments are validated with _schemas"

    (def doc ":zd/docname customers.uno\n&partners\n:rel tags.client")

    (matcho/assert
     {:status 422
      :body {:errors [{:type :doc-validation
                       :message "Expected type of 'set, got 'symbol"
                       :path [:zd/subdocs :partners :rel]}]}}
     (web/handle ztx 'zd/api
                 {:uri "/customers._draft/edit"
                  :request-method :put
                  :body (tutils/req-body doc)}))

    (def doc ":zd/docname customers.uno\n:rel #{rel.client}\n:desc \"best client!\"\n:title \"uno inc.\"\n&partners \n:rel #{rel.unknown}")

    (matcho/assert
     {:status 200}
     (web/handle ztx 'zd/api
                 {:uri "/customers._draft/edit"
                  :request-method :put
                  :body (tutils/req-body doc)}))

    (is (tutils/read-doc "customers/uno.zd"))

    (testing ":schema defined for a subdocument &mydoc in _schema.zd"

      (def doc ":zd/docname customers.uno\n:title \"uno inc\"\n:rel #{}\n:tags #{}\n&mydoc\n:rel #{tags.client}")

      (matcho/assert
       {:status 422,
        :body
        {:message "document validation failed",
         :docname "customers.uno",
         :errors
         [{:type :doc-validation,
           :message ":title is required",
           :path [:zd/subdocs :mydoc :title]}]}}
       (web/handle ztx 'zd/api
                   {:uri "/customers.uno/edit"
                    :request-method :put
                    :body (tutils/req-body doc)})))

    (def doc ":zd/docname customers.uno\n:title \"uno inc\"\n:desc \"uno incorporated\"\n:rel #{}\n&mydoc\n:rel #{tags.client}\n:title \"mytitle\"")

    (matcho/assert
     {:status 200}
     (web/handle ztx 'zd/api
                 {:uri "/customers.uno/edit"
                  :request-method :put
                  :body (tutils/req-body doc)})))

  (testing "cleanup and check"
    (matcho/assert
     {:status 200 :body "/customers"}
     (web/handle ztx 'zd/api {:uri "/customers.uno"
                              :request-method :delete}))

    (is (nil? (tutils/read-doc "customers/uno.zd")))))

(deftest schema-edit
  "schema is reloaded dynamically"

  ;; TODO test that :key and &subdoc schemas are applied

  (tutils/prepare! ztx)

  (def doc ":zd/docname partners.boom\n:title \"boom industries\"")

  (testing "root _schema requires :desc"
    (matcho/assert
     {:status 422
      :body {:errors [{:path [:desc]}]}}
     (web/handle ztx 'zd/api
                 {:uri "/partners._draft/edit"
                  :request-method :put
                  :body (tutils/req-body doc)})))

  (def sch ":zd/docname partners._schema\n:title \"Schema\"\n:desc /\nschema\n:tags #{}\n:schema {:require #{:category}}")

  (testing "add _schema for partners"
    (matcho/assert
     {:status 200}
     (web/handle ztx 'zd/api
                 {:uri "/partners._draft/edit"
                  :request-method :put
                  :body (tutils/req-body sch)}))
    (is (string? (tutils/read-doc "partners/_schema.zd"))))

  (testing ":category is now required in partners._schema"
    (def doc ":zd/docname partners.boom\n:title \"boom industries\"\n:tags #{}")

    (matcho/assert
     {:status 422}
     (web/handle ztx 'zd/api
                 {:uri "/partners._draft/edit"
                  :request-method :put
                  :body (tutils/req-body doc)}))

    (def doc ":zd/docname partners.boom\n:title \"boom industries\"\n:desc \"mydesc\"\n:category \"E-commerce\"")

    (matcho/assert
     {:status 200}
     (web/handle ztx 'zd/api
                 {:uri "/partners._draft/edit"
                  :request-method :put
                  :body (tutils/req-body doc)}))
    (is (string? (tutils/read-doc "partners/boom.zd"))))

  (testing "delete created docs"
    (matcho/assert
     {:status 200 :body string?}
     (web/handle ztx 'zd/api {:uri "/partners._schema"
                              :request-method :delete}))

    (is (nil? (tutils/read-doc "partners/_schema.zd")))

    (matcho/assert
     {:status 200 :body string?}
     (web/handle ztx 'zd/api {:uri "/partners.boom"
                              :request-method :delete}))
    (is (nil? (tutils/read-doc "partners/boom.zd")))))
