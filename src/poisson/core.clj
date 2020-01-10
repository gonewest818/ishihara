(ns poisson.core
  (:require [kdtree :as k]
            [quil.core :as q]
            [quil.middleware :as m]))

(declare make-kd-entry)
(declare make-sizes)
(declare random-disc)
(declare choose-color)

(defn setup
  "Provide the initial state map for this applet"
  []
  (q/frame-rate 15)
  (let [state {;;:kdtree          (k/build-tree [disc])
               ;;:points          (list disc)
               :building        true
               :max-tries       1000
               :color-generator :cosine
               :cmin            0.0
               :cmax            1.0
               ;; :a               [0.5 0.5 0.5]
               ;; :b               [0.5 0.5 0.5]
               ;; :c               [1.0 1.0 0.5]
               ;; :d               [0.8 0.9 0.3]
               :a               [0.578 0.188 0.448]
               :b               [0.127 0.288 0.373]
               :c               [1.918 0.677 0.529]
               :d               [-0.112 -0.533 0.568]
               :smin            3
               :slim            100
               :smax            100
               :sdec            5
               :offscreen       false}
        disc  (random-disc state)
        color (choose-color state)]
    (assoc state
           :kdtree (k/insert nil (make-kd-entry disc color))
           :points (list disc))))

(defn setup-offscreen
  "Initial state vector with :offscreen flag set true"
  []
  (assoc (setup) :offscreen true))

(defn settings
  "Processing documentation insists this must be done here"
  []
  (q/pixel-density 2)
  (q/smooth 16))

(defn make-sizes
  "Make a list of possible sizes for new discs"
  ;; for now just a list of steps spaced equally, but
  ;; could be augmented to something more interesting
  ;; like the golden ratio
  [{:keys [smin smax sdec]}]            ; does this need the full state vector?
  (range smin smax sdec))               ; todo: rename sdec

(defn random-disc
  "Randomly generate a disc's position and radius.
  Return a map with keys :x :y :r and :sizes"
  [state]
  (let [sizes (make-sizes state)]
   {:x (int (q/random 0 (q/width)))
    :y (int (q/random 0 (q/height)))
    :r (rand-nth sizes)
    :sizes sizes}))

(defn random-polar-disc
  "Randomly generate a disc's position given an origin and desired
  distance from that origin, as well as the new disc's radius. Return
  a map with keys :x :y :r and :sizes"
  [x y dist r state]
  (let [theta (q/random 0 (* 2.0 q/PI))]
    {:x (int (+ x (* dist (q/cos theta))))
     :y (int (+ y (* dist (q/sin theta))))
     :r (int r)
     :sizes (make-sizes state)}))

(defn sq-dist
  "Compute the squared distance from (x1,y1) to (x2,y2)"
  [x1 y1 x2 y2]
  (let [dx (- x1 x2)
        dy (- y1 y2)]
    (+ (* dx dx) (* dy dy))))

(defn collision?
  "Return a closure that serves as a predicate to test new points for
  potential collision with this disc."
  [{:keys [x y r]}]
  (fn [p]
    (let [[px py] p
          pr (:r (meta p))]
      (< (sq-dist x y px py)
         (* (+ pr r) (+ pr r))))))

(defn offscreen?
  "True if this disc does not intersect the screen's rectangle"
  [{:keys [x y r]}]
  (let [w (q/width)
        h (q/height)]
    (or (< x (- r))
        (> x (+ w r))
        (< y (- r))
        (> y (+ h r)))
    #_(or (< x 100)
        (> x 400)
        (< y 100)
        (> y 400))))

(defn make-interval
  "Make a square interval around the position of the supplied disc"
  [{:keys [x y]} pad]
  [[(- x pad) (+ x pad)] [(- y pad) (+ y pad)]])

(defn make-kd-entry
  "Make the object that lives in the kd-tree data structure"
  [disc color]
  (with-meta
    [(:x disc) (:y disc)]               ; 2 element vector
    {:r (:r disc)
     :color color}))

(defn random-color
  "Generate a random rgb triple whose channel values lie in the interval
  from :cmin to :cmax"
  [cmin cmax]
  [(q/random cmin cmax)
   (q/random cmin cmax)
   (q/random cmin cmax)])

(defn cosine-gradient-color
  "Cosine-based gradient generator as defined in
  http://www.iquilezles.org/www/articles/palettes/palettes.htm"
  [vec-a vec-b vec-c vec-d t]
  (map (fn [a b c d]
         (q/constrain (* 255.0 (+ a (* b (q/cos (* 2.0 q/PI (+ (* c t) d))))))
                      0.0 255.0))
       vec-a vec-b vec-c vec-d))

(defn choose-color
  "Choose a color with one of the available generators"
  [{:keys [color-generator cmin cmax a b c d]}]
  (case color-generator
    :random (random-color cmin cmax)
    :cosine (cosine-gradient-color a b c d (q/random cmin cmax))))

(defn update-state
  "Generate a new disc in the display area"
  [{:keys [kdtree points building smax max-tries], :as state}]
  (if-not building
    state
    ;; TODO: to generate new discs, select an existing one and
    ;; generate a new random radius and then randomly sample new
    ;; locations where the new disc just touches the existing disc.
    ;; If the selected location doesn't collide with anything else
    ;; then use that location.  Keep trying until a max is reached.
    ;; If max is reached then for this particular disc, set the
    ;; max available radius to be smaller than the one we just tested
    ;; and try again. If the max available radius is already the
    ;; smallest radius we allow, then remove this disc from further
    ;; consideration because all adjencies are full.
    (if (zero? (count points))          ; if no more discs can be generated
      (assoc state :building false)     ; toggle flag that we are done
      (let  [selected (rand-nth points)  ; else choose existing pt at random
             radius (rand-nth (:sizes selected)) ;(last (:sizes selected))
             dist     (+ radius (:r selected) 1.0) ; add an epsilon
             pad      (+ smax radius radius (:r selected) 1.0) ;add an epsilon
             ;; todo: use kd-tree to search existing discs in a neighborhood/interval
             ;; for now just search them all inefficiently
             existing
             (k/interval-search kdtree (make-interval selected pad))
             ;;(k/interval-search kdtree [[##-Inf ##Inf] [##-Inf ##Inf]])
             ]
        (loop [i 0]
          (let [disc  (random-polar-disc (:x selected) (:y selected) dist radius state)
                color (choose-color state)]
            (if (or (offscreen? disc)
                    (some (collision? disc) existing))
              (if (< i max-tries)       ; keep trying until limit is reached
                (recur (inc i))
                ;; we're at the limit of tries, so filter the list of sizes
                ;; (we're assuming a smaller disc might still fit here) and
                ;; if no smaller sizes are available then remove this point
                ;; from further consideration
                (let [new-sizes (filter #(< % radius) (:sizes selected))
                      new-selected (assoc selected :sizes new-sizes)
                      new-state (assoc state :points (remove #(= % selected) points))]
                  (if (pos? (count new-sizes))
                    (assoc new-state :points (conj (:points new-state) new-selected))
                    new-state)))
              ;; else we found a good disc, thus insert into points list and kdtree
              (assoc state
                     :points (conj points disc)
                     :kdtree (k/insert kdtree (make-kd-entry disc color))))))))))

(defn update-offscreen
  "For offscreen rendering we recursively call `update-state` until all
  discs are generated"
  [{:keys [building points], :as state}]
  (if building
    (do
      (->> (format "update-offscreen: %s" (count points))
           (q/print-every-n-millisec 1000))
      (recur (update-state state)))
    state))

(defn draw-state
  "Draw all discs"
  [{:keys [kdtree building slim smax offscreen], :as state}]
  (q/background 25)
  (q/no-stroke)
  ;; shouldn't there be a way to iterate the whole tree? is this slow?
  (doseq [p (k/interval-search kdtree [[##-Inf ##Inf] [##-Inf ##Inf]])]
    (let [[x y] p
          {:keys [r color]} (meta p)]
      (apply q/fill color)
      (q/ellipse x y (* 2 r) (* 2 r))))
  (if (and building (not offscreen))
    (do
      (q/fill 255 255 255)
      (q/text (format "building %s points %s" (:building state) (count (:points state))) 20 20))))

(defn draw-offscreen
  "Generate offscreen rendering context, then render, save, and exit"
  [state]
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

#_(q/defsketch poisson                    ; offscreen pdf
  :title "packed circles"
  :size [1500 1500]
  :setup setup-offscreen
  :settings settings
  :draw draw-offscreen
  :middleware [m/fun-mode])

(q/defsketch poisson                    ; onscreen animation
  :title "packed circles"
  :size [500 500]
  :setup setup
  :settings settings
  :update update-state
  :draw draw-state
  :features [:keep-on-top]
  :middleware [m/fun-mode])
