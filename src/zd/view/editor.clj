(ns zd.view.editor
  (:require [cheshire.core :as json]
            [clojure.string :as str]
            [hiccup.core]
            [hiccup.util]
            [zd.schema :as sch]
            [zd.view.icons :as icons]
            [zd.store :as store]
            [zen.core :as zen]))

(defn editor [ztx _ doc content]
  (let [header (str ":zd/docname " (:zd/docname doc) "\n")
        text (str header content)
        template (when (str/blank? content)
                   (sch/get-class-template ztx (:zd/parent doc)))
        symbols (store/symbols ztx)
        anns    (store/annotations ztx)
        zendoc-config (zen/get-symbol ztx (-> @ztx :zen/state :http :state :config :zendoc))
        zendoc {:text (if (str/blank? content)
                        (str text "\n" template)
                        text)
                :symbols symbols
                :keys (store/props ztx)
                :icons  icons/icons
                :annotations anns
                :validator (:validator zendoc-config)
                :doc (:zd/docname doc)}]
    [:script#editor-config (str "var zendoc=" (json/generate-string zendoc) ";")]))
