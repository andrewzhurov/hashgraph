(ns hashgraph.app.transitions
  (:require [hashgraph.utils.js-map :as js-map] :reload-all))

(defmacro t! [view-state & ts]
  (let [desired (gensym "desired")
        prop->t (gensym "prop->t")]
    `(let [~desired (->desired ~view-state)
           ~prop->t (->prop->t ~view-state)]
       ~(cons
         `do
         (apply concat
          (for [[prop time-start val-start val-end time-end] (partition 5 ts)]
            `((js-map/assoc! ~desired ~prop ~val-end)
              (js-map/update! ~prop->t ~prop
                              js-map/assoc!
                              :transition/time-start ~time-start
                              :transition/val-start ~val-start
                              :transition/val-end ~val-end
                              :transition/time-end ~time-end)))))
       (js-map/assoc! view-state->with-t? ~view-state true))))
