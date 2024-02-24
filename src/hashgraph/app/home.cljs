(ns hashgraph.app.home
  (:require [rum.core :as rum]
            [garden.core :as gc]
            [garden.selectors :as gs]
            [hashgraph.app.icons :as hga-icons]
            [hashgraph.app.view :as hga-view]))

(def desc-size "14px")
(def delim-size "18px")
(def delim-margin "20px")
(def home-styles
  [:.home {:width                 "100vw"
           :height                "100vh"
           :min-width             "100vw"
           :min-height            "100vh"
           :display               :grid
           :grid-template-columns "1fr auto 1fr"
           :grid-template-rows    "1fr 1fr 1fr 1fr 1fr 1fr 1fr 1fr 1fr 1fr 1fr 1fr 1fr 1fr 1fr"
           :gap                   "0px 0px"
           :grid-auto-flow        :row
           :grid-template-areas
           (apply str (map pr-str [". distributed-ledger ."
                                   ". distributed-ledger ."
                                   ". distributed-ledger ."
                                   ". virtual-voting ."
                                   ". virtual-voting ."
                                   ". virtual-voting ."
                                   ". hashgraph see-for-yourself"
                                   ". hashgraph see-for-yourself"
                                   ". hashgraph see-for-yourself"
                                   ". . ."
                                   ". . ."
                                   ". . ."
                                   ". . ."
                                   ". . ."]))}
   [:.distributed-ledger {:grid-area "distributed-ledger"}]
   [:.virtual-voting {:grid-area "virtual-voting"}]
   [:.hashgraph {:grid-area "hashgraph"}]
   [:.see-for-yourself {:grid-area       "see-for-yourself"
                        :display         :flex
                        :flex-direction  :row
                        :justify-content :center
                        :align-items     :center}
    [:svg {:margin-left "10px"}]]

   [:.def.distributed-ledger {:font-size "24px"}]
   [:.def.virtual-voting {:font-size "28px"}]
   [:.def.hashgraph {:font-size "36px"}]

   [:.def {:position        :relative
           :display         :flex
           :flex-direction  :column
           :align-items     :center
           :justify-content :center}
    [:.with-meta {:position :relative}
     [:.sym {:font-weight :bold
             :font-size   "1em"}]
     [:.meta {:font-size   "0.6em" #_desc-size
              :width       :fit-content
              :white-space :nowrap
              :position    :absolute
              :left        "50%"}
      [(gs/& (gs/not :.top)) {:bottom    "0px"
                              :transform "translate(-50%, 100%)"}]
      [(gs/& :.top) {:top "0px"
                     :transform "translate(-50%, -100%)"}]]]
    [:.next {:font-size "16px" #_"0.8em"
             :font-weight :bold
             :position  :absolute
             :bottom    "0px"
             :left      "50%"
             :transform "translate(-50%, calc(50% + 0.6em))"}]]
   ])

(def home-styles-css
  (gc/css home-styles))

(rum/defc view []
  [:<>
   [:style ]
   [:style home-styles-css]
   [:div.home
    [:div.def.distributed-ledger
     [:div.with-meta
      [:div.meta.top "Making a"]
      [:div.sym [:a {:href hga-view/dlt-link :target "_blank"} "Distributed Ledger"]]
      [:div.meta "having total order of transactions"]]
     [:div.next "powered by"]]
    [:div.def.virtual-voting
     [:div.with-meta
      [:div.sym [:a {:href hga-view/virtual-voting-link :target "_blank"} "Virtual Voting"]]
      [:div.meta "asynchronous byzantine fault-tolerant"]]
     [:div.next "atop"]]
    [:div.def.hashgraph
     [:div.with-meta
      [:div.sym [:a {:href hga-view/paper-link :target "_blank"} "Hashgraph"]]
      [:div.meta "of gossip"]]]

    [:div.see-for-yourself "Scary? Oh, sure! See for yourself " (hga-icons/icon :solid :right-long :size :2x :color :lightgray)]]])
