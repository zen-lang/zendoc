(ns zd.view.editor
  (:require [cheshire.core :as json]
            [hiccup.core]
            [hiccup.util]
            [zd.view.icons :as icons]
            [zd.store :as store]))


;; TODO: templates
(defn editor [ztx ctx {docname :zd/docname :as doc} content]
  (let [header (str ":zd/docname " (:zd/docname doc) "\n")
        text (str header content)
        symbols (store/symbols ztx)
        anns    (store/annotations ztx)
        zendoc {:text text
                :symbols symbols
                :keys (store/props ztx)
                :icons  icons/icons
                :annotations anns
                :doc (:zd/docname doc)}]
    [:script#editor-config (str "var zendoc=" (json/generate-string zendoc) ";")]))
