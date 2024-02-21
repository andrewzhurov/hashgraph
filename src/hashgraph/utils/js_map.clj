(ns hashgraph.utils.js-map
  "Clojure interface to js/Map, optimized with macro-time keyword-key->idx substitution.
   Be sure to not use with runtime resolvable keyword keys."
  (:refer-clojure :exclude [assoc! dissoc! get get-in empty? contains? keys])
  (:require [hashgraph.utils.js-map :as js-map] :reload-all))

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

(defmacro empty-js-map [] `(new js/Map))

(defmacro ensure-js-map [?js-map]
  `(or ~?js-map (empty-js-map)))

(defmacro js-map? [maybe-js-map]
  `(identical? js/Map (type ~maybe-js-map)))

(defmacro get [?js-map key]
  `(let [?js-map# ~?js-map]
     (when ?js-map#
       (.get ?js-map# ~(key->js-map-key key)))))

(defmacro get-in [?js-map [k & rest-ks]]
  (if (clojure.core/empty? rest-ks)
    `(get ~?js-map ~k)
    `(get-in (get ~?js-map ~k) ~rest-ks)))

(defmacro assoc!
  ([?js-map key val]
   `(let [js-map# (ensure-js-map ~?js-map)]
      (.set js-map# ~(key->js-map-key key) ~val)
      js-map#))
  ([?js-map key val & [next-key next-val & rest-kvs]]
   `(assoc! (assoc! ~?js-map ~key ~val)
            ~next-key ~next-val ~@rest-kvs)))

(defmacro js-map [& kvs]
  `(let [m# (empty-js-map)]
     (doseq [[k# v#] ~(->> kvs
                           (partition 2)
                           (mapv (fn [[k v]]
                                   [(key->js-map-key k) v])))]
       (assoc! m# k# v#))
     m#))

(defmacro assoc-in! [?js-map [k & rest-ks] val]
  (if (clojure.core/empty? rest-ks)
    `(assoc! ~?js-map ~k ~val)
    `(let [?js-map# ~?js-map]
       (assoc! ?js-map# ~k (assoc-in! (get ?js-map# ~k) ~rest-ks ~val)))))

(defmacro dissoc! [?js-map key]
  `(let [js-map# (ensure-js-map ~?js-map)]
     (.delete js-map# ~(key->js-map-key key))
     js-map#))

(defmacro update! [?js-map key with & args]
  `(assoc! ~?js-map ~key (~with (get ~?js-map ~key) ~@args)))

(defmacro update-in! [?js-map [k & rest-ks] with & args]
  (if (clojure.core/empty? rest-ks)
    `(let [?js-map# ~?js-map]
       (update! ?js-map# ~k ~with ~@args))
    `(let [?js-map# ~?js-map]
       (assoc! ?js-map# ~k (update-in! (get ?js-map# ~k) ~rest-ks ~with ~@args)))))


(defmacro empty? [js-map]
  `(zero? (.-size ~js-map)))

(defmacro contains?
  [js-map key]
  `(.has ~js-map ~(key->js-map-key key)))

(defmacro keys-impl
  "Returns keys as number (peek into implementation)"
  [js-map]
  `(cljs.core/js->clj (js/Array.from (.keys ~js-map))))

(defmacro js-map-key->keyword-key []
  (-> @*keyword-key->js-map-key
      (->> (into (hash-map) (map (fn [[kk jmk]] [jmk kk]))))))

(defmacro js-map->clj-map [?js-map]
  `(let [->keyword-key# (js-map-key->keyword-key)]
     (js-map/js-map->clj-map* ~?js-map ->keyword-key#)))

#_
(defmacro get!! [js-map key]
  `(let [js-map# (or ~js-map (js-map))
         v#      (or (get js-map# ~key)
                     (let [new-v# (js-map)]
                       (js-map/assoc! js-map# ~key new-v#)
                       new-v#))]
     v#))

#_
(defmacro get-in!! [?js-map [k & rest-ks]]
  (let [v (gensym "v")]
    `(let [~v (get!! ~?js-map ~k)]
       ~(if (clojure.core/empty? rest-ks)
          v
          `(get-in!! ~v ~rest-ks)))))

#_
(defmacro assoc!! [?js-map k v]
  `(let [js-map# (or ~?js-map (js-map))]
     (assoc! js-map# ~k ~v)))

#_
(defmacro dissoc!! [?js-map k]
  `(or (some-> ~?js-map (dissoc! ~k))
       (js-map)))

#_
(defmacro update!! [?js-map k with & args]
  `(let [js-map# (or ?js-map (js-map))]
     (assoc!! ~?js-map )))
