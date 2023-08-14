(ns zd.view.topbar
  (:require
   [zd.view.utils :as utils]
   [clojure.string :as str]
   [stylo.core :refer [c]]
   [clojure.pprint]
   [zd.store :as store]))

(defn actions [ztx {{uri :uri qs :query-string :as req} :request :as ctx} {docname :zd/docname :as doc}]
  (let [edit-btn
        [:a {:class (c [:text :gray-600] [:hover [:text :green-600]] [:ml 4])
             :href (str docname "/edit" "?" qs)}
         [:i.fas.fa-edit]]

        ;; TODO move to .js
        del-script
        (format "if (confirm(\"delete document?\") == true){
                  fetch('/%s', {method: 'DELETE'}).then((resp)=> {
                  resp.text().then((docid) => { window.location.pathname = docid})})}"
                docname)

        del-btn
        [:a {:class (c [:text :gray-600] [:hover [:text :red-600]] [:ml 4])
             :onclick (when-not (= docname 'index) del-script)}
         [:i.fas.fa-trash-xmark]]

        plus-btn
        [:a {:class (c :cursor-pointer
                       [:text :gray-600]
                       [:ml 4]
                       [:hover [:text :green-600]])
             :href (str docname "." "_draft/edit")}
         [:i.fas.fa-plus]]

        container
        [:div {:class (c :flex :items-center :border-l [:ml 4])}]]

    (conj container edit-btn del-btn plus-btn)))

(def separator [:span {:class (c [:mx 1.5] [:text :gray-500])} "/"])
(defn breadcrumbs [ztx ctx {docname :zd/docname :as doc}]
  (let [icon-class (c :cursor-pointer [:text :gray-500] [:hover [:text :orange-600]])]
    [:div {:class (c :flex [:py 2])}
     [:div {:class (c :flex :flex-flow :items-baseline [:space-x 2])}
      [:a {:href (str "/") :class icon-class}
       [:span.fa-regular.fa-house]]
      separator
      (->> (store/breadcrump ztx docname)
           (map (fn [docname]
                  [:div {:class (c :flex :flex-row :items-baseline)}
                   (utils/symbol-link ztx docname)]))
           (interpose separator))]]))

(defn topbar [ztx ctx doc]
  [:div {:class (c :flex :items-center)}
   (breadcrumbs ztx ctx doc)
   (actions ztx ctx doc)])
