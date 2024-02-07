(ns hashgraph.utils.js-map
  "Clojure interface to js/Map, optimized with macro-time keyword-key->idx substitution.
   Be sure to not use with runtime resolvable keyword keys."
  (:refer-clojure :exclude [assoc! dissoc! get empty? contains? keys]))

(def *keyword-key->js-map-key (atom (hash-map)))
(defn key->js-map-key [key]
  (if-not (keyword? key)
    key
    (if-let [js-map-key (@*keyword-key->js-map-key key)]
      js-map-key
      (let [new-js-map-key (inc (or (-> @*keyword-key->js-map-key
                                        vals
                                        sort
                                        last)
                                    0))]
        (swap! *keyword-key->js-map-key assoc key new-js-map-key)
        new-js-map-key))))

(defmacro js-map? [maybe-js-map]
  `(identical? js/Map (type ~maybe-js-map)))

(defmacro assoc! [js-map key val]
  `(do (.set ~js-map ~(key->js-map-key key) ~val)
       ~js-map))

(defmacro dissoc! [js-map key]
  `(do (.delete ~js-map ~(key->js-map-key key))
       ~js-map))

(defmacro update! [js-map key with & args]
  `(let [js-map# ~js-map]
     (assoc! js-map# ~key (~with (get js-map# ~key) ~@args))))

(defmacro js-map [& kvs]
  `(let [m# (new js/Map)]
     (doseq [[k# v#] ~(->> kvs
                         (partition 2)
                         (mapv (fn [[k v]]
                                 [(key->js-map-key k) v])))]
       (assoc! m# k# v#))
     m#))

(defmacro get [js-map key]
  `(.get ~js-map ~(key->js-map-key key)))

(defmacro empty? [js-map]
  `(zero? (.-size ~js-map)))

(defmacro contains?
  [js-map key]
  `(.has ~js-map ~(key->js-map-key key)))

(defmacro keys-impl
  "Returns keys as number (peek into implementation)"
  [js-map]
  `(cljs.core/js->clj (js/Array.from (.keys ~js-map))))
