(ns hashgraph.utils2
  #?(:cljs (:require-macros [cljs.core :as core]
                            [cljs.support :refer [assert-args]]))
  )

(defmacro time3
  "Evaluates expr and prints the time it took. Returns the value of expr."
  [expr]
  `(cljs.core/let [start# (cljs.core/system-time)
                   ret#   ~expr]
     (cljs.core/prn
      (cljs.core/str "Elapsed time: "
                     (.toFixed (cljs.core/- (cljs.core/system-time) start#) 6)
                     " msecs"))
     ret#))
#_(time)
(defmacro td
  "Timed debug, evaluates expr and inserts cljs debug if it took too much time. Returns the value of expression."
  [t expr]
  `(cljs.core/let [start# (cljs.core/system-time)
                   ret#   ~expr
                   end#   (cljs.core/system-time)
                   took#  (cljs.core/- (cljs.core/system-time) start#)]
     (js/console.log ~(pr-str expr) " => " ret#)
     (js/console.log (cljs.core/str "took: " (.toFixed took# 6) " ms"))
     (cljs.core/when (cljs.core/> took# ~t)
       (cljs.core/prn (cljs.core/str "that's a LOT, debugging"))
       ~(list 'js* "debugger;")
       ~expr)
     ;; (cljs.core/prn (cljs.core/- (cljs.core/system-time) start#))
     ret#
     ;; (cljs.core/or (cljs.core/and (cljs.core/> took# ~t)
     ;;                              (cljs.core/prn "LONG")
     ;;                              ;;~(list 'js* "debugger;")
     ;;                              ret#)
     ;;               ret#)
     )
  )
