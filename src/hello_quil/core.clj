(ns hello-quil.core
  (:require [quil.core :as q]
            [quil.middleware :as m]
            [clojure.math.numeric-tower :as math]))

;; From the lein new quil project-name 

;; From the ldnclj dojo on 31st March 2015
;; https://gist.github.com/rockBreaker/a899bc0b798866c3badf

(defn location []
  [(rand-int 255) (rand-int 255)])
 
(defn init-locations
  []
  (for [i (range 5)] (location)))
 
(defn dist-vector
  [[x1 y1] [x2 y2]]
  [(- x1 x2) (- y1 y2)])
 
(defn distance
  [b1 b2]
  (let [[dx dy] (dist-vector b1 b2)]
    (math/sqrt (+ (* dx dx) (* dy dy)))))
 
(defn vec-add
  [[x1 y1] [x2 y2]]
  [(+ x1 x2) (+ x2 y2)])
 
(defn rescale-vec
  [n v]
  (map #(/ % n) v))
 
(defn limit [v]
  (map #(mod % 500) v))
 
(defn update-locations
  [locations]
  (for [b locations]
    (->> 
     (filter #(not= b %) locations)
     (map #(dist-vector b %))
     (reduce vec-add)
     (rescale-vec (- (count locations) 1))
     (rescale-vec 1.5)
     (vec-add b)
     (map math/abs)
     (map math/round)
     (limit))))
 
(defn setup []
  ; Set frame rate to 30 frames per second.
  (q/frame-rate 30)
  ; Set color mode to HSB (HSV) instead of default RGB.
  (q/color-mode :hsb)
  ; setup function returns initial state. It contains
  ; circle color and position.
  {:color 0
   :angle 0
   :locations (init-locations)})
 
(defn update-state [state]
  ; Update sketch state by changing circle color and position.
  {:color (mod (+ (:color state) 0.7) 255)
   :angle (+ (:angle state) 0.1)
   :locations (update-locations (:locations state))})
 
(defn draw-state [state]
  ; Clear the sketch by filling it with light-grey color.
  (q/background 240)
  ; Set circle color.
 
(q/fill 200 200 100)
  
  (let [locations (:locations state)]
    (doall
     (for [[x y] locations]
       (q/ellipse x y 20 20)))))
 
(q/defsketch hello-quil
  :title "You spin my circle right round"
  :size [500 500]
  ; setup function called only once, during sketch initialization.
  :setup setup
  ; update-state is called on each iteration before draw-state.
  :update update-state
  :draw draw-state
  ; This sketch uses functional-mode middleware.
  ; Check quil wiki for more info about middlewares and particularly
  ; fun-mode.
  :middleware [m/fun-mode])
