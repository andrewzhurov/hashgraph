(ns hashgraph.utils.unique-cursor
  (:require [hashgraph.utils.core :refer [hash=]]))

(deftype UniqueCursor [ref path meta]
  Object
  (equiv [this other]
    (-equiv this other))

  IAtom

  IMeta
  (-meta [_] meta)

  IEquiv
  (-equiv [this other]
    (identical? this other))

  IDeref
  (-deref [_]
    (get-in (-deref ref) path))

  IWatchable
  (-add-watch [this key callback]
    (let [ref  (.-ref this)
          path (.-path this)]
      (add-watch ref key ;; this is the difference
                 (fn [_ _ oldv newv]
                   (let [old (get-in oldv path)
                         new (get-in newv path)]
                     (when-not (hash= old new)
                       (callback key this old new))))))
    this)

  (-remove-watch [this key]
    (let [ref (.-ref this)] ;; this is the difference
      (remove-watch ref key))
    this)

  IHash
  (-hash [this] (goog/getUid this))

  IReset
  (-reset! [_ newv]
    (swap! ref assoc-in path newv)
    newv)

  ISwap
  (-swap! [this f]
    (-reset! this (f (-deref this))))
  (-swap! [this f a]
    (-reset! this (f (-deref this) a)))
  (-swap! [this f a b]
    (-reset! this (f (-deref this) a b)))
  (-swap! [this f a b rest]
    (-reset! this (apply f (-deref this) a b rest)))

  IPrintWithWriter
  (-pr-writer [this writer opts]
    (-write writer "#object [rum.cursor.Cursor ")
    (pr-writer {:val (-deref this)} writer opts)
    (-write writer "]")))

(defn unique-cursor-in
  [ref path & {:as options}]
  (if (instance? UniqueCursor ref)
    (UniqueCursor. (.-ref ref) (into (.-path ref) path) (:meta options))
    (UniqueCursor. ref path (:meta options))))

(defn unique-cursor
  "Same as [[unique-cursor-in]] but accepts single key instead of path vector."
  [ref key & options]
  (apply unique-cursor-in ref [key] options))
