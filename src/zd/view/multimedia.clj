(ns zd.view.multimedia
  (:require
   [zd.methods :as methods]
   [stylo.core :refer [c]]
   [clojure.string :as str]))


(defn render-yt
  "renders video player from link"
  [link & [opts]]
  (when-not (str/blank? link)
    [:div {:class (c [:px 0] [:py 2] [:bg :white])}
     (if (or (str/starts-with? link "https://youtu.be")
             (str/starts-with? link "https://www.youtube.com"))
       [:iframe {:src (str "https://www.youtube.com/embed/" (last (str/split link #"/")))
                 :width (or  (:width opts) "560")
                 :height (or (:height opts) "315")
                 :frameborder 0
                 :allow "accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture"}]
       [:div
        [:video {:width "100%" :height "300px" :controls "controls"}
         [:source {:src link :type "video/mp4"}]]])]))

(defmethod methods/inline-method :youtube
  [ztx m d ctx]
  (render-yt d))
