(ns hashgraph.app.icons
  (:require ["@fortawesome/react-fontawesome" :refer [FontAwesomeIcon] :as fa]
            ["@fortawesome/free-regular-svg-icons" :as far]
            ["@fortawesome/free-solid-svg-icons" :as fas]
            ["@fortawesome/fontawesome-svg-core" :refer [library] :as fa-core]
            [goog.object]))

(defn icon-key->icon-name [icon-key]
  (apply str "fa" (-> (name icon-key)
                      (clojure.string/split #"-")
                      (->> (map clojure.string/capitalize)))))

(defn icon [icon-style icon-key & [icon-size]]
  (let [icon-name (icon-key->icon-name icon-key)]
    (js/React.createElement
     FontAwesomeIcon
     #js {:icon (case icon-style
                  :solid   (goog.object/get fas icon-name)
                  :regular (goog.object/get far icon-name))
          :size (or (some-> icon-size name)
                    "1x")})))

(defn transfer [opts]
  [:svg
   (merge {:xmlns   "http://www.w3.org/2000/svg",
           :width   "800px",
           :height  "800px",
           :viewBox "0 0 48 48"}
          opts)
   [:title "transfer"]
   [:g {:style {:transform "scale(0.75)"
                :transform-origin :center}}
    [:g
     [:rect {:width "48", :height "48", :fill "none"}]]
    [:g
     [:path
      {:d
       "M19,26a2,2,0,0,0-2,2V40H7V28a2,2,0,0,0-4,0V42a2,2,0,0,0,2,2H19a2,2,0,0,0,2-2V28A2,2,0,0,0,19,26Z"}]
     [:path
      {:d
       "M43,26a2,2,0,0,0-2,2V40H31V28a2,2,0,0,0-4,0V42a2,2,0,0,0,2,2H43a2,2,0,0,0,2-2V28A2,2,0,0,0,43,26Z"}]
     [:path
      {:d
       "M34,16v1.2l-2.6-2.6a1.9,1.9,0,0,0-3,.2,2.1,2.1,0,0,0,.2,2.7l6,5.9a1.9,1.9,0,0,0,2.8,0l6-5.9a2.1,2.1,0,0,0,.2-2.7,1.9,1.9,0,0,0-3-.2L38,17.2V16a14,14,0,0,0-28,0v6a2,2,0,0,0,4,0V16a10,10,0,0,1,20,0Z"}]]]])
