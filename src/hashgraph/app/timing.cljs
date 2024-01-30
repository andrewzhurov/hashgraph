(ns hashgraph.app.timing
  (:require [cljs.math :refer [floor ceil pow]]))

;; https://gizma.com/easing/#easeInOutCubic
(defn ease-in-out-cubic [x]
  (if (< x 0.5)
    (* 4 x x x)
    (-> 1
        (- (/ (pow (-> -2 (* x) (+ 2)) 3)
              2)))))

;; https://gizma.com/easing/#easeOutQuint
(defn ease-out-quint [x]
  (-> 1
      (- (pow (-> 1 (- x)) 5))))

(defn sharp-in-sharp-out [x]
  #_
  (letl [x x
         p1 (min 1 (/ x (/ 1 5)))
         p2 (min 1 (max 0 (-> x (- (/ 1 5)) (/ (/ 1 5)))))
         p3 (min 1 (max 0 (-> x (- (/ 2 5)) (/ (/ 1 5)))))
         p4 (min 1 (max 0 (-> x (- (/ 3 5)) (/ (/ 1 5)))))
         p5 (min 1 (max 0 (-> x (- (/ 4 5)) (/ (/ 1 5)))))
         p1-out (-> p1 ease-out-quint (/ 5))
         p2-out (-> p2 (/ 5))
         p3-out (-> p3 ease-out-quint (/ 5))
         p4-out (-> p4 (/ 5))
         p5-out (-> p5 ease-out-quint (/ 5))
         out* (+ p1-out
                 p2-out
                 p3-out
                 p4-out
                 p5-out)
         out x]
        out)
  x
  #_(-> x
      (+ (pow (min x (/ 1 5)) 1.5))
      (- (pow (min x (/ 2 5)) 1.5))
      #_(- (pow (min x (/ 4 5)) 10))))
