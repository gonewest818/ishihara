(ns ishihara.core
  (:require [kdtree :as k]
            [quil.core :as q]
            [quil.middleware :as m]
            [clojure.tools.cli :refer [parse-opts]]
            [clj-async-profiler.core :as prof])
  (:gen-class))

(declare make-kd-entry)
(declare make-sizes)
(declare random-disc)
(declare choose-color)

(defn make-setup
  "Provide the initial state map for this applet"
  [rmin rmax rincr seed offscreen]
  (fn []
    (q/random-seed seed)
    (q/noise-seed seed)
    (q/frame-rate 60)
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
                 :offscreen       offscreen
                 :seed            seed}
          disc  (random-disc state)
          color (choose-color state (/ (:x disc) (q/width)))]
      (assoc state
             :kdtree (k/insert nil (make-kd-entry disc color))
             :points (list disc)))))

(defn settings
  "Processing documentation insists this must be done here"
  []
  (q/smooth 16))

(defn make-sizes
  "Make a list of possible sizes for new discs"
  ;; for now just a list of steps spaced equally, but
  ;; could be augmented to something more interesting
  ;; like the golden ratio
  [{:keys [rmin rmax rincr]}            ; state
   {:keys [x y]}]                       ; a disc
  (let [mx (* 2.0 q/PI 0.005 x)
        my (* 2.0 q/PI 0.005 y)
        upper (q/constrain
               (+ rmin
                  (* (q/noise (* x 0.005) (* y 0.005))         ; sine waves 0.25 (+ 2.0 (q/sin mx) (q/sin my))
                     (- rmax rmin)))
               rmin rmax)
        lower (-> upper
                  (- (* 0.3 (- rmax rmin)))
                  (q/constrain rmin upper))]
    (range lower upper rincr)))

(defn random-disc
  "Randomly generate a disc's position and radius.
  Return a map with keys :x :y :r and :sizes"
  [state]
  (let [pt {:x (int (q/random 0 (q/width)))
            :y (int (q/random 0 (q/height)))}
        sizes (make-sizes state pt)]
    (assoc pt :sizes sizes :r (nth sizes (q/random (count sizes))))))

(defn random-polar-disc
  "Randomly generate a disc's position given an origin and desired
  distance from that origin, as well as the new disc's radius. Return
  a map with keys :x :y :r and :sizes"
  [x y dist r state]
  (let [theta (q/random 0 (* 2.0 q/PI))]
    {:x (int (+ x (* dist (q/cos theta))))
     :y (int (+ y (* dist (q/sin theta))))
     :r (int r)
     :sizes (make-sizes state {:x x :y y})}))

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
  [{:keys [color-generator cmin cmax a b c d]} t]
  (case color-generator
    :random (random-color cmin cmax)
    :cosine (cosine-gradient-color a b c d t)))

(defn update-state
  "Generate a new disc in the display area"
  [{:keys [kdtree points building rmax max-tries], :as state}]
  (if-not building
    state
    ;; NOTE: to generate new discs, select an existing one and
    ;; generate a new random radius and then randomly sample new
    ;; locations where the new disc just touches the existing disc.
    (if (zero? (count points))          ; if no more discs can be generated
      (assoc state :building false)     ; toggle flag that we are done
      (let  [selected (nth points (q/random (count points)))  ; else choose existing pt at random
             radius   (nth (:sizes selected) (q/random (count (:sizes selected))))
             dist     (+ radius (:r selected) 1.0) ; add an epsilon
             pad      (+ rmax radius radius (:r selected) 1.0) ;add an epsilon
             existing (k/interval-search kdtree (make-interval selected pad))]
        (loop [i 0]
          (let [disc  (random-polar-disc (:x selected) (:y selected) dist radius state)
                color (choose-color state (* 1.5 (/ (:r disc) rmax)))]
            ;; If the selected location collides with another then
            ;; make another random choice
            (if (or (offscreen? disc)
                    (some (collision? disc) existing))
              (if (< i max-tries)
                (recur (inc i))
                ;; If max is reached then for this particular disc, set the
                ;; max available radius to be smaller than the one we just tested
                ;; and try again. If the max available radius is already the
                ;; smallest radius we allow, then remove this disc from further
                ;; consideration because all adjencies are full.
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
          {:keys [r color]} (meta p)]
      ;; (apply q/fill (conj (vec color) 1))
      ;; (q/ellipse x y (* 2 rmax) (* 2 rmax))
      (apply q/fill color)
      (q/ellipse x y (* 2 r) (* 2 r))
      ))
  (if (and building (not offscreen))
    (do
      (q/fill 0)
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

(defn cli-usage
  [summary]
  (println "Options:\n" summary))

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
    (let [{:keys [width height render rmin rmax rincr output seed]} options
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
                  :setup (make-setup rmin rmax rincr seed true)
                  :settings settings
                  :draw draw-offscreen
                  :middleware [m/fun-mode])
        "java2d" (q/sketch
                  :title "ishihara diagram"
                  :size [width height]
                  :setup (make-setup rmin rmax rincr seed false)
                  :settings settings
                  :update update-state
                  :draw draw-state
                  :features [:keep-on-top]
                  :middleware [m/fun-mode])))))

;; manual profiling
;; (prof/start)
;; (prof/stop)
