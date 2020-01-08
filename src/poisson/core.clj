(ns poisson.core
  (:require [kdtree :as k]
            [quil.core :as q]
            [quil.middleware :as m]))

(def smin 3)
(def smax 25)
(def smax-sq (* smax smax))
(def max-tries 1000)

(defn setup []
  (q/frame-rate 30)
  (q/background 24)
  (q/smooth)
  {:kdtree (k/build-tree [])
   :building true})

(defn random-disc []
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
         (+ (* r r) (* pr pr))))))

(defn update-state [{:keys [kdtree building]}]
  (if building
    (loop [i 0]
      (let [disc (random-disc)
            radius (+ smax (:r disc))
            points (k/interval-search kdtree
                                      [[(- (:x disc) radius)
                                        (+ (:x disc) radius)]
                                       [(- (:y disc) radius)
                                        (+ (:y disc) radius)]])]
        (if (some (collision? disc) points)
          (if (< i max-tries)
            (recur (inc i))
            {:kdtree kdtree :building false})
          {:kdtree (k/insert kdtree (with-meta [(:x disc) (:y disc)] {:r (:r disc)}))
           :building true})))
    {:kdtree kdtree :building false}))

(defn draw-state [{:keys [kdtree building]}]
  ;;(q/stroke-weight 5)
  ;;(q/stroke (:color state) 128 255 60)
  (q/stroke 128 128 128)
  (q/fill 255 255 255)
  (doseq [p (k/interval-search kdtree [[0 (q/width)] [0 (q/height)]])]
    (let [[x y] p
          r (:r (meta p))]
      (q/ellipse x y (* 2 r) (* 2 r))
      ;;(q/ellipse x y r r)
      )))

(q/defsketch poisson
  :title "packed circles"
  :size [500 500]
  :setup setup
  :update update-state
  :draw draw-state
  :features [:keep-on-top]
  :middleware [m/fun-mode])
