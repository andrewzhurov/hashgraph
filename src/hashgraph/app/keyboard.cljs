(ns hashgraph.app.keyboard
  (:require [rum.core :as rum]
            [clojure.set :as set]
            [hashgraph.utils.core :refer-macros [l] :as utils]))

(def event-key->kb-key
  {"Shift"         :shift
   "GroupPrevious" :shift
   "Alt"           :alt
   "Meta"          :meta
   "Control"       :ctrl
   "ArrowRight"    :->
   "ArrowLeft"     :<-
   " "             :space})

(defn e->kb-key [keyboard-event] (event-key->kb-key (.-key keyboard-event)))

;; pressed keyboard keys
(def *kb-keys (atom #{}))
(defn kb-key!   [k] (swap! *kb-keys conj k))
(defn unkb-key! [k] (swap! *kb-keys disj k))

(defn kb-keys? [kb-keys & [current-kb-keys]]
  (assert (set? kb-keys))
  (let [current-kb-keys (or current-kb-keys
                            (if rum/*reactions*
                              (rum/react *kb-keys)
                              @*kb-keys))]
    (set/subset? kb-keys current-kb-keys)))

(defn kb-key? [kb-key & [current-kb-keys]] (kb-keys? #{kb-key} current-kb-keys))

(defn register-keydowns-lister!
  []
  (.addEventListener js/document "keydown" (fn [e]
                                             (when-let [kb-key (e->kb-key e)]
                                               (kb-key! kb-key))))
  (.addEventListener js/document "keyup"   (fn [e]
                                             (when-let [kb-key (e->kb-key e)]
                                               (unkb-key! kb-key)))))


(defonce *shortcut->action (atom {}))
(defonce *shortcut->action-ordered
  (rum/derived-atom [*shortcut->action] ::derive-ordered-shortcuts
                    (fn [shortcut->action]
                      (->> shortcut->action
                           (sort-by (comp count key) >)
                           (apply concat)
                           (apply array-map)))))

(defn reg-shortcut! [shortcut action]
  (swap! *shortcut->action assoc shortcut action))

(defn unreg-shortcut! [shortcut]
  (swap! *shortcut->action dissoc shortcut))

(add-watch *kb-keys ::maybe-activate-shortcut
           (fn [_ _ _ kb-keys]
             (when-let [action (->> @*shortcut->action-ordered
                                    (some (fn [[shortcut action]]
                                            (when (kb-keys? shortcut kb-keys)
                                              action))))]
               (action))))
