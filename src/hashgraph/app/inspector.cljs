(ns hashgraph.app.inspector)

#_
[:div.debug {:style {:width       "50vw"
                     :height      "90vh"
                     :white-space "pre-wrap"}}
 (when hovered-evt
   (with-out-str (cljs.pprint/pprint hovered-evt)))]
