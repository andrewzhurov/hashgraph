(ns hashgraph.app.inspector
  (:require [hashgraph.utils.core :as utils]))

(defmacro inspectable* [el & [opts]]
  (cond-> {:class    ["inspectable"
                      `(when (and (->analysis?)
                                 (not (->in-inspected? ~el))
                                 (not (->in-peeked? ~el)))
                        "dimm")
                      `(when (and (->analysis?) (->inspected? ~el)) "inspected")
                      `(when (and (->analysis?) (->peeked? ~el)) "peeked")]
           :on-click `(fn [e#] (when (key-mod? inspect-key)
                                 #_#_(.preventDefault e#)
                                 (.stopPropagation e#)
                                 (toggle-inspect! ~el)))}
    opts (assoc :on-mouse-move `(fn [e#] (.stopPropagation e#)
                                     (active-inspectable! (get ~opts :path) ~el))
                :on-mouse-leave `(fn [] (inactive-inspectable! (get ~opts :path) ~el)))))

(defmacro inspectable-el [value opts & el-builder]
  `(let [el#                   (binding [*inspectable-nested-depth* (inc *inspectable-nested-depth*)] ~@el-builder)
         tag#                  (first el#)
         inspectable-attr-map# (inspectable ~value (assoc ~opts
                                                          :nested?      (some? *inspectable-nested-depth*)
                                                          :nested-depth (or *inspectable-nested-depth* 0)))]
     (if-not (map? (second el#))
       (into [tag# inspectable-attr-map#] (rest el#))
       (into [tag# (utils/merge-attr-maps (second el#) inspectable-attr-map#)] (rest (rest el#))))))
