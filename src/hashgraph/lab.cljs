(ns hashgraph.lab
  "A place to test the universe to get insight. A playground.")

(let [l1 (list 1 2 3)]
  (= l1 (-> l1 rest (conj 1))))

(let [r (doall (range 100000))
      r2 (-> r (rest) (conj 0))
      m {r :val}]
  (time (get m r)))

(identical? (drop 1 (drop 1 (list 1 2 3 4)))
            (drop 2 (list 1 2 3 4)))



(let [r (range 100000)]
  (time (transduce (comp (filter even?)
                         (map inc))
                   +
                   (range 10000)))
  (time (reduce ((comp (filter even?)
                       (map inc))
                 +)
                (range 10000)))
  (time (-> r (->> (filter even?)
                   (map inc)
                   (reduce +))))
  )


#_
(profile
 {}
 (let [arg [(set (range 0 10000))
            (set (range 5000 15000))
            (set (range 10000 20000))
            ]]
   (doall (repeatedly 100 #(p ::union (doall (apply clojure.set/union arg)))))
   (doall (repeatedly 100 #(p ::reduce (doall (reduce into #{} arg)))))
   :finished))
;; union is generally faster than reduce
;; TODO check sets vs transducers for difference vs distinct + remove
