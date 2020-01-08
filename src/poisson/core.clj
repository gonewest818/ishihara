(ns poisson.core
  (:require [kdtree :as k]
            [quil.core :as q]
            [quil.middleware :as m]))

(defn setup []
  (q/frame-rate 15)
  {:kdtree (k/build-tree [])
   :building true
   :max-tries 10000
   :cmin 64
   :cmax 255
   :smin 2
   :slim 50
   :smax 50
   :offscreen false})

(defn setup-offscreen []
  (println "setup-offscreen")
  (assoc (setup) :offscreen true))

(defn settings []
  (q/pixel-density 2)
  (q/smooth 16))

(defn random-disc [{:keys [smin slim]}]
  {:x (q/random 0 (q/width))
   :y (q/random 0 (q/height))
   :r (q/random smin slim)})

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

(defn make-interval [{:keys [x y]} radius]
  [[(- x radius) (+ x radius)]
   [(- y radius) (+ y radius)]])

(defn choose-color [{:keys [cmin cmax]}]
  [(q/random cmin cmax)
   (q/random cmin cmax)
   (q/random cmin cmax)])

(defn update-state [{:keys [kdtree building smin slim smax max-tries], :as state}]
  (if-not building
    state
    (loop [i 0]
      (let [;disc (random-disc state)
            disc (assoc (random-disc state) :r slim)
            radius (+ smax (:r disc))
            points (k/interval-search kdtree (make-interval disc radius))]
        (if (some (collision? disc) points)
          (if (< i max-tries)
            (recur (inc i))
            (if (> slim smin)
              (assoc state :slim (dec slim))
              (assoc state :building false)))
          (assoc state :kdtree (k/insert kdtree
                                         (with-meta
                                           [(:x disc) (:y disc)]
                                           {:r (:r disc)
                                            :color (choose-color state)}))))))))

(defn update-offscreen [{:keys [building slim], :as state}]
  (q/print-every-n-millisec 1000 (format "update-offscreen building: %s lim: %s" building slim))
  (if building
    (recur (update-state state))
    state))

(defn draw-state [{:keys [kdtree building slim offscreen]}]
  (q/background 24)
  (q/stroke-weight 0)
  (doseq [p (k/interval-search kdtree [[0 (q/width)] [0 (q/height)]])]
    (let [[x y] p
          {:keys [r color]} (meta p)]
      (apply q/fill color)
      (q/ellipse x y (* 2 r) (* 2 r))))
  (if (and building (not offscreen))
    (do
      (q/fill 255 255 255)
      (q/text (format "active: %s radius: %s" building slim) 10 20))))

(defn draw-offscreen [state]
  (println "draw-offscreen: begin")
  (let [gr (q/create-graphics (q/width) (q/height)
                              :pdf "generated/create-graphics.pdf")]
    (q/with-graphics gr
      (-> state
          update-offscreen
          draw-state)
      (.dispose gr)))
  (q/exit)
  (println "draw-offscreen: done"))

(q/defsketch poisson                  ; offscreen pdf
  :title "packed circles"
  :size [1200 600]
  :setup setup-offscreen
  :settings settings
  :draw draw-offscreen
  :middleware [m/fun-mode])

#_(q/defsketch poisson                    ; onscreen animation
  :title "packed circles"
  :size [1000 1000]
  :setup setup
  :settings settings
  :update update-offscreen
  :draw draw-state
  :features [:keep-on-top]
  :middleware [m/fun-mode])
