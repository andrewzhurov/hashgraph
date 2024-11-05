(ns hashgraph.app.transitions
  (:require [hashgraph.utils.js-map :as js-map] :reload-all))

(defmacro ->view-state [view-id-path]
  `(js-map/get-in! view-id-path->view-state ~view-id-path (js-map/js-map)))

(defmacro t! [view-state & ts]
  (let [desired (gensym "desired")
        prop->t (gensym "prop->t")]
    `(let [~desired (view-state->desired ~view-state)
           ~prop->t (view-state->prop->t ~view-state)]
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
