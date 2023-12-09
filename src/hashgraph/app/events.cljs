(ns hashgraph.app.events
  (:require [hashgraph.main :as hg]
            [hashgraph.members :as hg-members]))

(def evts-init #{})
(defonce *evts (atom evts-init))

(defn issue! []
  (swap! *evts (fn [evts]
                 (let [sender       (rand-nth (keys hg-members/members))
                       receiver     (rand-nth (keys (dissoc hg-members/members sender)))
                       sender-tip   (some (fn [evt] (when (= (:creator evt) sender) evt)) (hg/tips evts))
                       receiver-tip (some (fn [evt] (when (= (:creator evt) receiver) evt)) (hg/tips evts))
                       new-evt      (if (nil? sender-tip)
                                      {:creator sender}
                                      (cond-> {:creator receiver
                                               :parent sender-tip}
                                        receiver-tip (assoc :self-parent receiver-tip)))]
                   (if (or (nil? sender-tip)
                           ;; tip not known yet
                           (not (->> evts (filter #(= (:creator %) receiver)) (some #(= (:parent %) sender-tip)))))
                     (conj evts new-evt)
                     (recur evts))))))
