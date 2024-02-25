(ns hashgraph.app.icons
  (:require ["@fortawesome/react-fontawesome" :refer [FontAwesomeIcon] :as fa]
            ["@fortawesome/free-regular-svg-icons" :as far]
            ["@fortawesome/free-solid-svg-icons" :as fas]
            ["@fortawesome/free-brands-svg-icons" :as fabr]
            ["@fortawesome/fontawesome-svg-core" :refer [library] :as fa-core]
            [goog.object]))

(defn svg-fa [{:keys [size style]} & content]
  (into [:svg.svg-inline--fa {:xmlns   "http://www.w3.org/2000/svg",
                              :viewBox "0 0 48 48"
                              :class   (str "fa-" size)
                              :fill    (:color style)}]
        content))

(defn transfer [opts]
  (svg-fa
   opts
   [:title "transfer"]
   [:g {:style {:transform-origin :center
                :scale            "0.9"}}
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
       "M34,16v1.2l-2.6-2.6a1.9,1.9,0,0,0-3,.2,2.1,2.1,0,0,0,.2,2.7l6,5.9a1.9,1.9,0,0,0,2.8,0l6-5.9a2.1,2.1,0,0,0,.2-2.7,1.9,1.9,0,0,0-3-.2L38,17.2V16a14,14,0,0,0-28,0v6a2,2,0,0,0,4,0V16a10,10,0,0,1,20,0Z"}]]]))

(defn poor-connectivity [opts]
  (svg-fa
   opts
   [:g {:id "SVGRepo_bgCarrier", :stroke-width "0"}]
   [:g
    {:id              "SVGRepo_tracerCarrier",
     :stroke-linecap  "round",
     :stroke-linejoin "round"}]
   [:g
    {:id "SVGRepo_iconCarrier"}
    [:g
     {:fill  "#2e3436"
      :style {:scale "2.5"}}
     [:path
      {:d
       "m 5 7 c -0.554688 0 -1 0.445312 -1 1 v 6 c 0 0.554688 0.445312 1 1 1 h 1 c 0.554688 0 1 -0.445312 1 -1 v -6 c 0 -0.554688 -0.445312 -1 -1 -1 z m -4 3 c -0.554688 0 -1 0.445312 -1 1 v 3 c 0 0.554688 0.445312 1 1 1 h 1 c 0.554688 0 1 -0.445312 1 -1 v -3 c 0 -0.554688 -0.445312 -1 -1 -1 z m 0 0"}]
     [:path
      {:d
       "m 13 1 c -0.554688 0 -1 0.445312 -1 1 v 12 c 0 0.554688 0.445312 1 1 1 h 1 c 0.554688 0 1 -0.445312 1 -1 v -12 c 0 -0.554688 -0.445312 -1 -1 -1 z m -4 3 c -0.554688 0 -1 0.445312 -1 1 v 9 c 0 0.554688 0.445312 1 1 1 h 1 c 0.554688 0 1 -0.445312 1 -1 v -9 c 0 -0.554688 -0.445312 -1 -1 -1 z m 0 0",
       :fill-opacity "0.34902"}]]]))

(defn icon-key->icon-name [icon-key]
  (apply str "fa" (-> (name icon-key)
                      (clojure.string/split #"-")
                      (->> (map clojure.string/capitalize)))))

(defn icon [icon-style icon-key & {:keys [size color]
                                   :or {size  :1x
                                        color "black"}}]
  (let [icon-opts {:size  (name size)
                   :style {:color color}}]
    (case icon-key
      :transfer (transfer icon-opts)
      :poor-connectivity (poor-connectivity icon-opts)

      (let [icon-name (icon-key->icon-name icon-key)
            icon (case icon-style
                   :solid   (goog.object/get fas icon-name)
                   :regular (goog.object/get far icon-name)
                   :brands  (goog.object/get fabr icon-name))]
        (js/React.createElement
         FontAwesomeIcon
         (-> icon-opts
             (assoc :icon icon)
             clj->js))))))
