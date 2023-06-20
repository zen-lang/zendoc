(ns zen-web.routemap-test
  (:require [zen.core :as zen]
            [zen-web.core :as web]
            [zen-web.routemap :as routemap]
            [clojure.test :refer [deftest is testing]]
            [matcho.core :as matcho]))

(defmethod web/resolve-route
  'myweb/custom-api
  [_ztx cfg path ctx]
  (-> (assoc ctx :op 'myweb/custom-match)
      (update :path into path)
      (update :resolution-path into (into [(:zen/name cfg)] path))))

(defmethod web/routes
  'myweb/custom-api
  [_ztx cfg ctx]
  [(-> (assoc ctx :op 'myweb/custom-match)
       (update :path into [:* :GET])
       (update :by conj (:zen/name cfg)))])

(def ztx (zen/new-context {}))

(zen/load-ns
 ztx
 '{ns myweb
   import #{zen-web}

   index
   {:zen/tags #{zen/op zen-web/op}
    :engine zen-web.engines/response
    :response {:status 200
               :body {:message "Hello!"}}}

   well-known
   {:zen/tags #{zen/op zen-web/op}
    :engine zen-web.engines/response
    :response {:status 200
               :body {:message "WK"}}}

   get-pt
   {:zen/tags #{zen/op zen-web/op}
    :engine zen-web.engines/response
    :response {:status 200
               :body {:message "PT"}}}

   get-users
   {:zen/tags #{zen/op zen-web/op}
    :engine zen-web.engines/response
    :response {:status 200
               :body {:message "Users"}}}

   get-user
   {:zen/tags #{zen/op zen-web/op}
    :engine zen-web.engines/response
    :response {:status 200
               :body {:message "User"}}}

   admin-api
   {:zen/tags #{zen-web/api}
    :engine zen-web/routemap
    :mw [zen-web/debug-middleware]
    "users" {:GET get-users
             [:id] {:GET get-user}}}

   custom
   {:zen/tags #{zen/op zen-web/op}
    :engine zen-web.engines/response
    :response {:status 200
               :body {:message "custom"}}}

   custom-api
   {:zen/tags #{zen-web/api zen-web/op}
    :zen/desc "just custom api - match all routes with custom-match"}

   tenant-pt
   {:zen/tags #{zen/op zen-web/op}
    :engine zen-web.engines/response
    :response {:status 200
               :body {:message "tenant-pt"}}}

   default
   {:zen/tags #{zen/op zen-web/op}
    :engine zen-web.engines/response
    :response {:status 200}}

   route
   {:zen/tags #{zen-web/api}
    :engine zen-web/routemap
    :apis [zen-web/rpc-api]
    :mw []
    :GET index
    "static" {:* {:mw [zen-web/debug-middleware]
                  :GET default}}
    ".well-known" {:GET well-known}
    "Patient" {[:id] {:GET get-pt}}
    "custom" {:apis [custom-api]}
    "admin" {:apis [admin-api]}
    [:tenant] {"patients" {:GET tenant-pt}}}})

(deftest routes-matching

  (is (empty? (zen/errors ztx)))

  (is (nil? (web/*resolve-route ztx 'myweb/route {:path ["undefined" :GET]})))

  ;; test root path
  (matcho/match
   (web/*resolve-route ztx 'myweb/route [:GET])
    {:op 'myweb/index
     :resolution-path ['myweb/route :GET],
     :middlewares empty?})

  ;; test simple match
  (matcho/match
   (web/*resolve-route ztx 'myweb/route  [".well-known" :GET])
    {:op 'myweb/well-known
     :resolution-path ['myweb/route ".well-known" :GET],
     :middlewares empty?})

  ;; test match with params
  (matcho/match
   (web/*resolve-route ztx 'myweb/route ["Patient" "pt-1" :GET])
    {:op 'myweb/get-pt
     :params {:id "pt-1"}
     :resolution-path ['myweb/route "Patient" [:id] :GET],
     :middlewares empty?})

  ;; test two route engines
  (matcho/match
   (web/*resolve-route ztx 'myweb/route  ["custom" "ups" :GET])
   {:op 'myweb/custom-match
    :path ["custom" "ups" :GET]
    :resolution-path ['myweb/route "custom" 'myweb/custom-api "ups" :GET]})

  ;; test two route maps
  (matcho/match
   (web/*resolve-route ztx 'myweb/route  ["admin" "users" :GET])
    {:path ["admin" "users" :GET],
     :middlewares ['zen-web/debug-middleware],
     :resolution-path ['myweb/route "admin" 'myweb/admin-api "users" :GET],
     :op 'myweb/get-users})

  (matcho/match
   (web/*resolve-route ztx 'myweb/route  ["admin" "users" "u-1" :GET])
    {:path ["admin" "users" :id :GET],
     :params {:id "u-1"},
     :middlewares ['zen-web/debug-middleware],
     :resolution-path ['myweb/route "admin" 'myweb/admin-api "users" [:id] :GET],
     :op 'myweb/get-user})

  ;; test root api
  (matcho/match
   (web/*resolve-route ztx 'myweb/route  [:POST])
    {:path [:POST],
     :resolution-path ['myweb/route 'zen-web/rpc-api :POST],
     :op 'zen-web/rpc})

  ;; test wildcard matching
  (matcho/match
   (web/*resolve-route ztx 'myweb/route ["static" "file.txt" :GET])
    {:path ["static" "file.txt" :GET],
     :params {:* ["file.txt"]}
     :middlewares ['zen-web/debug-middleware],
     :resolution-path ['myweb/route "static" :* :GET],
     :op 'myweb/default})

  (matcho/match
   (web/*resolve-route ztx 'myweb/route ["static" "content" "jpg" "image.jpg" :GET])
    {:path ["static" "content" "jpg" "image.jpg" :GET],
     :params {:* ["content" "jpg" "image.jpg"]}
     :middlewares ['zen-web/debug-middleware],
     :resolution-path ['myweb/route "static" :* :GET],
     :op 'myweb/default})

  (is (nil? (web/*resolve-route ztx 'myweb/route ["static"])))

  (is (nil? (web/*resolve-route ztx 'myweb/route ["static" "file.txt" :POST]))))

(deftest routes-list
  (matcho/match
   (web/*routes ztx 'myweb/route)
    [{:path [".well-known" :GET],
      :by ['myweb/route ".well-known" :GET],
      :op 'myweb/well-known}
     {:path [:GET],
      :by ['myweb/route :GET],
      :op 'myweb/index}
     {:path [:POST],
      :by ['myweb/route 'zen-web/rpc-api :POST],
      :op 'zen-web/rpc}
     {:path [:tenant "patients" :GET],
      :params #{:tenant},
      :by ['myweb/route [:tenant] "patients" :GET],
      :op 'myweb/tenant-pt}
     {:path ["Patient" :id :GET],
      :params #{:id},
      :by ['myweb/route "Patient" [:id] :GET],
      :op 'myweb/get-pt}
     {:path ["admin" "users" :GET],
      :middlewares ['zen-web/debug-middleware],
      :by ['myweb/route "admin" 'myweb/admin-api "users" :GET],
      :op 'myweb/get-users}
     {:path ["admin" "users" :id :GET],
      :middlewares ['zen-web/debug-middleware],
      :params #{:id},
      :by ['myweb/route "admin" 'myweb/admin-api "users" [:id] :GET],
      :op 'myweb/get-user}
     {:path ["custom" :* :GET],
      :by ['myweb/route "custom" 'myweb/custom-api],
      :op 'myweb/custom-match}]))

(deftest matching-order

  (zen/load-ns
   ztx
   '{ns matching-test
     import #{zen-web}

     default
     {:zen/tags #{zen/op zen-web/op}
      :engine zen-web.engines/response
      :response {:status 200}}

     myapi
     {:zen/tags #{zen-web/api}
      :engine zen-web/routemap
      [:oth-param] {:GET default}
      "exact2" {:GET default}}

     route
     {:zen/tags #{zen-web/api}
      :engine zen-web/routemap
      :apis [myapi]
      "exact" {:GET default}
      [:myparam] {:GET default}}})

  (is (empty? (zen/errors ztx)))

  ;; exact match always comes first
  (matcho/match
   {:path ["exact2" :GET],
    :params {},
    :middlewares [],
    :resolution-path ['matching-test/route 'matching-test/myapi "exact2" :GET],
    :op 'matching-test/default}
    (web/*resolve-route ztx 'matching-test/route ["exact2" :GET]))

  (matcho/match
   {:path ["exact" :GET],
    :params {},
    :middlewares [],
    :resolution-path ['matching-test/route "exact" :GET],
    :op 'matching-test/default}
   (web/*resolve-route ztx 'matching-test/route ["exact" :GET]))

  ;; matches :myparam from root before :oth-param from :apis
  (matcho/match
   {:path [:myparam :GET],
    :params {:myparam "a-param"},
    :middlewares [],
    :resolution-path ['matching-test/route [:myparam] :GET],
    :op 'matching-test/default}
   (web/*resolve-route ztx 'matching-test/route ["a-param" :GET])))
