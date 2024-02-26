(ns hashgraph.app.smooth-render
  (:require [goog.object]
            [clojure.set :as set]
            [rum.core :as rum]
            [hashgraph.app.styles :refer [reg-styles!]]
            [hashgraph.app.view :refer [t]]))

(def smooth-tt 250)
(def smooth-render-styles
  [[:.smooth-render
    [:>div {:opacity    0
            :transition (t :opacity smooth-tt smooth-tt)}]
    [:.showing {:opacity 1}]
    [:.hiding {:opacity 0}]]])

(reg-styles! ::smooth-render smooth-render-styles)


(defn maybe-hide-keys! [smooth-comp to-maybe-hide-keys]
  (let [t (cljs.core/system-time)
        *state (-> smooth-comp
                   .-state
                   (goog.object/get ":rum/state"))
        *hidden? (volatile! false)]
    (vswap! *state update ::key->state
            (fn [key->state]
              (reduce (fn [key->state-acc to-maybe-hide-key]
                        (let [?state   (get key->state-acc to-maybe-hide-key)
                              to-hide? (and ?state
                                            (= (::t-state ?state) ::hiding)
                                            (>= t (::hiding-t-end ?state)))]
                          (if to-hide?
                            (do (vreset! *hidden? true)
                                (dissoc key->state-acc to-maybe-hide-key))
                            key->state-acc)))
                      key->state
                      to-maybe-hide-keys)))
    (when @*hidden? (rum/request-render smooth-comp))))

;; TODO Enhancement: mount comps only after prev unmounted
;; I.e., do the render in two phases: 1) hide no-more-active 2) show novel active
(rum/defcs view < rum/static
  {:will-mount   (fn [state]
                   (let [key->state (->> (:rum/args state) (into (hash-map) (map (fn [comp] [(.-key comp) (hash-map ::comp  comp ::t-state ::to-show)]))))]
                     (assoc state
                            ::key->state key->state
                            ::new-keys-to-show (keys key->state))))

   :will-remount (fn [old-state new-state]
                   (let [old-key->state   (::key->state old-state)

                         new-comps        (:rum/args new-state)

                         old-keys         (set (keys old-key->state))
                         new-keys         (->> new-comps (into #{} (map (fn [comp] (.-key comp)))))
                         old-keys-to-hide (set/difference old-keys new-keys)
                         new-keys-to-show (set/difference new-keys old-keys)

                         hiding-t-end     (+ (cljs.core/system-time) smooth-tt)
                         old-key->state*  (reduce (fn [old-key->state-acc old-key-to-hide]
                                                    (update old-key->state-acc old-key-to-hide assoc ::t-state ::hiding ::hiding-t-end hiding-t-end))
                                                  old-key->state
                                                  old-keys-to-hide)

                         new-key->state  (reduce
                                          (fn [old-key->state-acc new-comp]
                                            (let [key (.-key new-comp)]
                                              (if-let [old-state (get old-key->state-acc key)]
                                                (assoc old-key->state-acc key (assoc old-state ::t-state ::showing ::comp new-comp))
                                                (assoc old-key->state-acc key (hash-map ::t-state ::to-show ::comp new-comp)))))
                                          old-key->state*
                                          new-comps)]

                     (when (not (empty? old-keys-to-hide))
                       (js/setTimeout #(maybe-hide-keys! (:rum/react-component new-state) old-keys-to-hide) smooth-tt))

                     (assoc new-state
                            ::key->state new-key->state
                            ::new-keys-to-show new-keys-to-show)))}

  {:after-render (fn [state]
                   (let [new-keys-to-show (::new-keys-to-show state)]
                     (if (empty? new-keys-to-show)
                       state
                       (let [old-key->state (::key->state state)
                             new-key->state (reduce (fn [new-key->state-acc new-key-to-show]
                                                      (update new-key->state-acc new-key-to-show assoc ::t-state ::showing))
                                                    old-key->state
                                                    new-keys-to-show)]
                         (-> state
                             (assoc ::key->state new-key->state)
                             (assoc ::request-render? true)
                             (dissoc ::new-keys-to-show))))))}
  {:after-render (fn [state]
                   (if (::request-render? state)
                     (do (js/setTimeout #(rum/request-render (:rum/react-component state)))
                         (dissoc state ::request-render?))
                     state))}

  [{::keys [key->state] :as state} _]
  [:div.smooth-render
   (for [[key {::keys [comp t-state]}] key->state]
     [:div {:key   key
            :class [(when (= ::showing t-state) "showing")]}
      comp])])
