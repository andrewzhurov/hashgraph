(ns hashgraph.app.tutorial
  (:require [rum.core :as rum]
            [hashgraph.app.utils :as hga-utils]))

(rum/defc view []
  [:div#tutorial
   (hga-utils/plug "tutorial")])
