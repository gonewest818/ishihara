(ns ishihara.core
  (:require [kdtree :as k]
            [quil.core :as q]
            [quil.middleware :as m]
            [clojure.string]
            [clojure.tools.cli :refer [parse-opts]]
            [clj-async-profiler.core :as prof])
  (:gen-class))

(defn perlin-field
  "Evaluate Perlin noise at a 2D point. Return the value
  as well as the local gradient."
  [x y noisedx noisedy]
  (let [x0 (* x noisedx)
        y0 (* y noisedy)
        x1 (* (+ x 1) noisedx)
        y1 (* (+ y 1) noisedy)
        v00 (q/noise x0 y0)
        v10 (q/noise x1 y0)
        v01 (q/noise x0 y1)]
    {:value v00
     :dvdx (- v10 v00)
     :dvdy (- v01 v00)}))

(defn make-sizes
  "Make a list of possible sizes for new discs"
  ;; for now just a list of steps spaced equally, but
  ;; could be augmented to something more interesting
  ;; like the golden ratio
  [{:keys [rmin rmax rincr rvar]} noise]
  (let [upper (-> (+ rmin (* noise (- rmax rmin)))
                  (q/constrain rmin rmax))
        lower (-> (* upper rvar)
                  (q/constrain rmin upper))]
    (if (< lower upper)
      (range lower upper rincr)
      (list lower))))                   ; avoid empty list

(defn random-disc
  "Randomly generate a disc's position and radius.
  Return a map with keys :x :y :r and :sizes"
  [state]
  (let [x (int (q/random 0 (q/width)))
        y (int (q/random 0 (q/height)))
        perlin (perlin-field x y (:noisedx state) (:noisedy state))
        sizes (make-sizes state (:value perlin))
        r (nth sizes (q/random (count sizes)))]
    {:x x
     :y y
     :r r
     :dx (:dvdx perlin)
     :dy (:dvdy perlin)
     :sizes sizes}))

(defn random-polar-disc
  "Randomly generate a disc's position given an origin and desired
  distance from that origin, as well as the new disc's radius. Return
  a map with keys :x :y :r and :sizes"
  [x y dist r state]
  (let [theta (q/random 0 (* 2.0 q/PI))
        perlin (perlin-field x y (:noisedx state) (:noisedy state))]
    {:x (int (+ x (* dist (q/cos theta))))
     :y (int (+ y (* dist (q/sin theta))))
     :r (int r)
     :dx (:dvdx perlin)
     :dy (:dvdy perlin)
     :sizes (make-sizes state (:value perlin))}))

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
        (> y (+ h r)))))

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
     :dx (:dx disc)
     :dy (:dy disc)
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
         (-> (+ a (* b (q/cos (* 2.0 q/PI (+ (* c t) d)))))
             (* 255.0)
             (q/constrain 0.0 255.0)))
       vec-a vec-b vec-c vec-d))

(defn choose-color
  "Choose a color with one of the available generators"
  [{:keys [color-generator cmin cmax a b c d rmax]}  ; state
   {:keys [r]}]                                      ; disc
  (case color-generator
    :random (random-color cmin cmax)
    :cosine (cosine-gradient-color a b c d (* 1.5 (/ r rmax)))))

(defn update-state
  "Generate a new disc in the display area"
  [{:keys [kdtree points building rmax max-tries epsilon], :as state}]
  (if-not building
    state
    ;; NOTE: to generate new discs, select an existing one and
    ;; generate a new random radius and then randomly sample new
    ;; locations where the new disc just touches the existing disc.
    (if (zero? (count points))          ; if no more discs can be generated
      (assoc state :building false)     ; toggle flag that we are done
      (let  [selected (nth points (q/random (count points)))  ; else choose existing pt at random
             radius   (nth (:sizes selected) (q/random (count (:sizes selected))))
             dist     (+ radius (:r selected) epsilon)
             pad      (+ rmax radius radius (:r selected) epsilon)
             existing (k/interval-search kdtree (make-interval selected pad))]
        (loop [i 0]
          (let [disc  (random-polar-disc (:x selected) (:y selected) dist radius state)
                color (choose-color state disc)]
            ;; If the selected location collides with another or
            ;; is offscreen then make another random choice
            (if (or (offscreen? disc)
                    (some (collision? disc) existing))
              (if (< i max-tries)
                (recur (inc i))
                ;; If max is reached then for this particular disc, set the
                ;; max available radius to be smaller than the one we just tested
                ;; and try again. If the max available radius is already the
                ;; smallest radius we allow, then remove this disc from further
                ;; consideration because the area is full.
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
  [{:keys [kdtree building rmax offscreen], :as state}]
  (q/background 25)
  (q/no-stroke)
  ;; shouldn't there be a way to iterate the whole tree? is this slow?
  (doseq [p (k/interval-search kdtree [[##-Inf ##Inf] [##-Inf ##Inf]])]
    (let [[x y] p
          {:keys [r color dx dy]} (meta p)]
      ;;(apply q/fill (conj (vec color) (/ (* r 64) rmax)))
      ;;(q/ellipse x y (* 2 rmax) (* 2 rmax))
      (apply q/fill color)
      ;;(q/ellipse x y 1 1)
      ;;(q/ellipse x y (* 2 r) (* 2 r))
      (q/with-translation [x y]
        (q/with-rotation [(q/atan2 dy dx)]
          (q/ellipse 0 0 (* 2 r) r)))
      ))
  (if (and building (not offscreen))
    (do
      (q/fill 128)
      (q/text (format "building %s points %s" (:building state) (count (:points state))) 20 20))))

(defn draw-offscreen
  "Generate offscreen rendering context, then render, save, and exit"
  [state]
  (println "draw-offscreen: begin")
  (let [gr (q/current-graphics)]
    (q/with-graphics gr
      (-> state
          update-offscreen
          draw-state)
      (.dispose gr)))
  (q/exit)
  (println "draw-offscreen: done"))

(defn make-setup
  "Provide the initial state map for this applet"
  [{:keys [render
           rmin rmax rincr rvar
           noisedx noisedy
           epsilon
           seed]}]
  (fn []
    (q/random-seed seed)
    (q/noise-seed seed)
    (q/frame-rate 120)
    (let [state {:building        true
                 :max-tries       1000
                 :color-generator :cosine
                 :cmin            0.0
                 :cmax            1.0
                 ;; :a               [ 0.578  0.188  0.448]
                 ;; :b               [ 0.127  0.288  0.373]
                 ;; :c               [ 1.918  0.677  0.529]
                 ;; :d               [-0.112 -0.533  0.568]
                 :a [(+ 0.5 (/ (q/random-gaussian) 5.0))
                     (+ 0.5 (/ (q/random-gaussian) 5.0))
                     (+ 0.5 (/ (q/random-gaussian) 5.0))]
                 :b [(+ 0.5 (/ (q/random-gaussian) 2.0))
                     (+ 0.5 (/ (q/random-gaussian) 2.0))
                     (+ 0.5 (/ (q/random-gaussian) 2.0))]
                 :c [(+ 0.5 (/ (q/random-gaussian) 6.0))
                     (+ 0.5 (/ (q/random-gaussian) 6.0))
                     (+ 0.5 (/ (q/random-gaussian) 6.0))]
                 :d [(/ (q/random-gaussian) 6.0)
                     (/ (q/random-gaussian) 6.0)
                     (/ (q/random-gaussian) 6.0)]
                 :rmin            rmin
                 :rmax            rmax
                 :rincr           rincr
                 :rvar            rvar
                 :epsilon         epsilon
                 :noisedx         (if (pos? noisedx)
                                    (/ 1.0 noisedx)
                                    (* 0.005 (+ 2.0 (q/random-gaussian))))
                 :noisedy         (if (pos? noisedy)
                                    (/ 1.0 noisedy)
                                    (* 0.005 (+ 2.0 (q/random-gaussian))))
                 :offscreen       (= render "pdf")
                 :seed            seed}
          disc  (random-disc state)
          color (choose-color state disc)]
      (assoc state
             :kdtree (k/insert nil (make-kd-entry disc color))
             :points (list disc)))))

(defn settings
  "Processing documentation insists this must be done here"
  []
  (q/pixel-density (q/display-density))
  (q/smooth 16))

(defn cli-usage
  [summary]
  (println (str "Options:\n" summary)))

(def cli-options
  [["-r" "--render RENDERER" "Choose between java2d or pdf"
    :default "java2d"
    :validate [#{"java2d" "pdf"} "Renderer must be java2d or pdf"]]

   ["-x" "--width X" "Width of output image"
    :default 500
    :parse-fn #(Integer/parseInt %)
    :validate [#(< 0 % 65536) "Must be between 1 and 65535"]]
   ["-y" "--height Y" "Height of output image"
    :default 500
    :parse-fn #(Integer/parseInt %)
    :validate [#(< 0 % 65536) "Must be between 1 and 65535"]]

   ["-l" "--rmin RMIN" "Minimum radius"
    :default 2
    :parse-fn #(Integer/parseInt %)
    :validate [#(< 0 % 65536) "Must be between 1 and 65535"]]
   ["-u" "--rmax RMAX" "Maximum radius"
    :default 125
    :parse-fn #(Integer/parseInt %)
    :validate [#(< 0 % 65536) "Must be between 1 and 65535"]]
   ["-i" "--rincr RINCR" "Increments between each radius"
    :default 1
    :parse-fn #(Integer/parseInt %)
    :validate [#(< 0 % 65536) "Must be between 1 and 65535"]]
   ["-v" "--rvar RVAR" "Variance of any given disc radius"
    :default 0.75
    :parse-fn #(Float/parseFloat %)
    :validate [#(pos? %) "Must be a positive value"]]
   ["-e" "--epsilon EPSILON" "Spacing between discs"
    :default 1
    :parse-fn #(Integer/parseInt %)
    :validate [#(< 0 % 65536) "Must be between 1 and 65535"]]

   ["-p" "--noisedx NDX" "Scale of Perlin noise in X (0 means random)"
    :default 200.0
    :parse-fn #(Float/parseFloat %)]
   ["-q" "--noisedy NDY" "Scale of Perlin noise in Y (0 means random)"
    :default 200.0
    :parse-fn #(Float/parseFloat %)]

   ["-o" "--output FILENAME" "Filename to write (pdf renderer only)"
    :default "generated/ishihara-%DATE%-%SEED%.pdf"]
   ["-s" "--seed VALUE" "Random number seed (System/currentTimeMillis)"
    :default (System/currentTimeMillis)
    :parse-fn #(Long/parseLong %)
    :validate [#(<= Long/MIN_VALUE % Long/MAX_VALUE) "Seed must be a valid long int"]]
   ["-h" "--help"]])

(defn -main [& args]
  (let [{:keys [options errors summary]} (parse-opts args cli-options)]
    ;; handle exceptions
    (when errors
      (println (String/join "\n" errors) "\n")
      (cli-usage summary)
      (System/exit -1))
    (when (:help options)
      (cli-usage summary)
      (System/exit 0))
    ;; else go ahead and run
    (let [{:keys [width height render output seed]} options
          date (.. java.time.LocalDate now toString)
          file (-> output
                   (clojure.string/replace #"%SEED%" (str seed))
                   (clojure.string/replace #"%DATE%" date))]
      (case render
        "pdf"    (q/sketch
                  :title "ishihara diagram offscreen"
                  :size [width height]
                  :renderer :pdf
                  :output-file file
                  :setup (make-setup options)
                  :settings settings
                  :draw draw-offscreen
                  :middleware [m/fun-mode])
        "java2d" (q/sketch
                  :title "ishihara diagram"
                  :size [width height]
                  :setup (make-setup options)
                  :settings settings
                  :update update-state
                  :draw draw-state
                  :features [:keep-on-top]
                  :middleware [m/fun-mode])))))

;; manual profiling
;; (prof/start)
;; (prof/stop)
