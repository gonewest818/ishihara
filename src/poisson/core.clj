(ns poisson.core
  (:require [kdtree :as k]
            [quil.core :as q]
            [quil.middleware :as m]))

(def max-tries 1000)
(def cmin 64)
(def cmax 255)

(defn setup []
  (q/frame-rate 30)
  {:kdtree (k/build-tree [])
   :building true
   :smin 3
   :smax 25})

(defn settings []
  (q/pixel-density 2)
  (q/smooth 16))

(defn random-disc [{:keys [smax]}]
  {:x (q/random 0 (q/width))
   :y (q/random 0 (q/height))
   :r (q/random smin smax)})

(defn sq-dist [x1 y1 x2 y2]
  (let [dx (- x1 x2)
        dy (- y1 y2)]
    (+ (* dx dx) (* dy dy))))

(defn collision? [{:keys [x y r]}]
  (fn [p]
    (let [[px py] p
          pr (:r (meta p))]
      (< (sq-dist x y px py)
         (* (+ pr r) (+ pr r))))))

(defn update-state [{:keys [kdtree building smax], :as state}]
  (if-not building
    state
    (loop [i 0]
      (let [disc (random-disc state)
            radius (+ smax (:r disc))
            points (k/interval-search kdtree
                                      [[(- (:x disc) radius)
                                        (+ (:x disc) radius)]
                                       [(- (:y disc) radius)
                                        (+ (:y disc) radius)]])]
        (if (some (collision? disc) points)
          (if (< i max-tries)
            (recur (inc i))
            (assoc state :building false))
          (assoc state :kdtree (k/insert kdtree (with-meta
                                                  [(:x disc) (:y disc)]
                                                  {:r (:r disc)
                                                   :color [(q/random cmin cmax)
                                                           (q/random cmin cmax)
                                                           (q/random cmin cmax)]}))))))))

(defn draw-state [{:keys [kdtree]}]
  (q/background 24)
  (q/stroke-weight 0)
  (doseq [p (k/interval-search kdtree [[0 (q/width)] [0 (q/height)]])]
    (let [[x y] p
          r (:r (meta p))
          color (:color (meta p))]
      (apply q/fill color)
      (q/ellipse x y (* 2 r) (* 2 r))
      ;;(q/ellipse x y r r)
      )))

(q/defsketch poisson
  :title "packed circles"
  :size [500 500]
  :setup setup
  :settings settings
  :update update-state
  :draw draw-state
  :renderer :opengl
  :features [:keep-on-top]
  :middleware [m/fun-mode])
