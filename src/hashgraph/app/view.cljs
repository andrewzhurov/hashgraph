(ns hashgraph.app.view
  (:require [cljs.math :refer [floor ceil]]
            [clojure.test :refer [deftest testing is are]]
            [hashgraph.members :as hg-members]
            [hashgraph.utils.core :refer [log!] :refer-macros [l letl] :as utils]))

(def issue-link "https://github.com/andrewzhurov/hashgraph/issues")
(def discussions-link "https://github.com/andrewzhurov/hashgraph/discussions")
(def view-mode-horizontal? true)
(def border-radius 6)
(def tutorial-size 300)
(def tutorial-margin 5)

(defn ceil-even [num]
  (let [ceiled (ceil num)]
    (cond-> ceiled
      (odd? ceiled) inc)))

(def window-height js/window.innerHeight)
(def window-width js/window.innerWidth)
(def window-size (if view-mode-horizontal? window-width window-height))
(def viz-size (/ (if view-mode-horizontal? window-height window-width) 3) #_(* (count hg-members/names) (+ evt-s hgs-padding)))
(def hgs-size (/ viz-size (count hg-members/names)))
(def evt-s        (-> hgs-size (/ 3) ceil-even))
(def hgs-padding  (-> hgs-size (- evt-s) (/ 2)))

(def evt-r (/ evt-s 2))
(def sp-padding  (* evt-s 1.5))

(def evt-see-size (-> evt-r (* 2) (- 2)))

;; flip coords in horizontal view
(if-not view-mode-horizontal?
  (do (def x  :x)
      (def x1 :x1)
      (def x2 :x2)
      (def cx :cx)
      (def y  :y)
      (def y1 :y1)
      (def y2 :y2)
      (def cy :cy))
  (do (def x  :y)
      (def x1 :y1)
      (def x2 :y2)
      (def cx :cy)
      (def y  :x)
      (def y1 :x1)
      (def y2 :x2)
      (def cy :cx)))

(def control-icon-size 36)
(def control-size 56)
(def scrollbar-height 15)
(def control-margin 5)

(def evt-offset (ceil (+ evt-s sp-padding)))
(def avatar-size 40)

(def members-height 66 #_(+ members-padding-y avatar-size members-padding-y))
(def members-padding-y (-> members-height (- avatar-size) (/ 2)))

(def load-area-size (-> tutorial-size
                        (/ 2)
                        (+ tutorial-margin)) #_(+ sp-padding evt-s))
(def after-viz-buffer-size (* 35 evt-offset))
(def playback-size (ceil (-> window-size
                             (- members-padding-y avatar-size members-padding-y load-area-size))))

(def wit-r (* 2 evt-r))
(def wit-s (* 2 wit-r))
(def evt-initial-offset-y (ceil hgs-padding))


(def vote-r evt-r)
(def vote-circumferance (* 2 js/Math.PI vote-r))
#_(def vote-circumferance
  (memoize
   (fn [event]
     (let [member (-> event hg/event->member)]
       (-> vote-circumferance
           (/ hg/total-stake)
           (* (:member/stake member)))))))
(def vote-stroke-width (+ vote-r vote-r))

(def idx->x
  (fn [idx]
    (-> idx
        (* (+ hgs-padding evt-s hgs-padding))
        (+ hgs-padding evt-r))))

(def evt->x
  (fn [evt]
    (-> evt :event/creator hg-members/member-name->person :member/idx idx->x)))

(def t->y
  (fn [t]
    t))

(def evt->y
  (memoize ;; over time will become slower to lookup than to calc
   (fn [evt]
     (-> evt
         :event/creation-time
         t->y
         (+ evt-initial-offset-y)))))

(defn evt->viz-height [event]
  (-> (some-> event
              evt->y
              (+ members-height)
              (+ load-area-size))
      (+ after-viz-buffer-size)))


(defn ->viz-viewbox-bound-min [viz-scroll]
  viz-scroll)

(defn ->viz-viewbox-bound-max [viz-scroll]
  (+ viz-scroll window-size))

(defn ->before-viz-viewbox? [y viz-scroll]
  (< y (->viz-viewbox-bound-min viz-scroll)))

(defn ->after-viz-viewbox? [y viz-scroll]
  (> y (->viz-viewbox-bound-max viz-scroll)))

(defn ->after-viz-playback-viewbox? [y viz-scroll]
  (let [playback-view-bound-max (+ viz-scroll playback-size)]
    (> y playback-view-bound-max)))


(defn t [& t-descs]
  (let [[first-part* & rest-parts] (utils/partition-at-with keyword? (rest t-descs))
        parts                      (cons (cons (first t-descs) first-part*) rest-parts)]
    (->> parts
         (map (fn [t-desc] (->> t-desc
                                (map (fn [t-desc-el] (cond (keyword? t-desc-el) (name t-desc-el)
                                                           (number? t-desc-el) (str t-desc-el "ms")
                                                           :else t-desc-el)))
                                (clojure.string/join " " ))))
         (clojure.string/join ", "))))

(deftest t-test
  (is (= (t :opacity 100 "ease"
            :r 200 "easeOut" 100
            :color 400 "easeIn" 300)
         "opacity 100ms ease, r 200ms easeOut 100ms, color 400ms easeIn 300ms")))
