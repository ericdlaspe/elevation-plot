(ns elevation-plot.core
  (:gen-class)
  (:require
   [taoensso.truss :as truss :refer (have have?)]
   [clojure.data.csv :as csv]
   [clojure.java.io :as io]
   [clojure.math.numeric-tower :as m]
   [clojure.tools.trace :as tr]
   [com.rpl.specter :as sp]
   [quil.core :as q]))

;;; Named colors in HSB( 360 100 100 1.0 )
(def colors {:black [0 0 0]
             :red [0 100 100]
             :blue [240 100 100]})

(defn get-hue
  [color]
  (first (color colors)))

(defn z-height->hue-rainbow
  "Return a color from the rainbow color scheme, based on z-height relative to
   `z-min` and `z-max`."
  [z z-min z-max]
  (if (<= z-min z z-max)
    [(q/map-range z z-min z-max (get-hue :blue) (get-hue :red)) 100 100]  ; z is not nil
    (:black colors)))  ; z is nil or 0

(defn row-data-good?
  "Returns true if every item in the `row` is a non-empty string;
   false otherwise."
  [row]
  (every? #(and (seq %) (string? %)) row))

(defn remove-incomplete-rows
  "Given `coll` is a vector of vectors, filters-out the nested vectors that have
   fewer elements than the header vector."
  [coll]
  ;; count the columns of the header row
  (let [n-columns ((comp count first) coll)]
    (filter (fn [row]
              (and (= n-columns (count row)) ; correct number of columns
                   (row-data-good? row))) ; good data in each item in the row
            coll)))

(defn clean-csv-data
  [coll]
  (remove-incomplete-rows coll))

(defn load-csv
  "Returns a vector of vectors loaded from the CSV file at `csv-path`."
  [csv-path]
  (with-open [reader (io/reader csv-path)]
    (doall (csv/read-csv reader))))

(defn strings->doubles
  [row]
  (mapv #(Double/parseDouble %) row))

(comment
  (strings->doubles ["39.661031139" "-84.025190726" "276.5"]))

(defn drop-header-row
  "Returns a copy of the CSV data in `coll` with the header row dropped."
  [coll]
  (drop 1 coll))

(defn csv-strings->numbers
  "Convert the strings in the CSV data `col` to numbers and return a vector of
   row vectors."
  [coll]
  (->> coll
       (map (fn [[x y z]] [(Double/parseDouble x)
                           (Double/parseDouble y)
                           (Float/parseFloat z)]))
       vec))

(def m-per-deg-lat
  "Returns the number of meters in one degree of latitude at the latitude
   angle `phi` on the WGS84 spheroid. This calculation is correct to within
   one centimeter according to
   https://en.wikipedia.org/wiki/Geographic_coordinate_system"
  (memoize
   (fn [phi]
     (+ 111132.92
        (* -559.82 (q/cos (* 2 phi)))
        (* 1.175 (q/cos (* 4 phi)))
        (* -0.0023 (q/cos (* 6 phi)))))))

(def m-per-deg-lon
  "Returns the number of meters in one degree of longitude at the longitude
   angle `phi` on the WGS84 spheroid. This calculation is correct to within
   one centimeter according to
   https://en.wikipedia.org/wiki/Geographic_coordinate_system"
  (memoize
   (fn [phi]
     (+ (* 111412.84 (q/cos phi))
        (* -93.5 (q/cos (* 3 phi)))
        (* 0.118 (q/cos (* 5 phi)))))))

;; (defn point-grid
;;   "Return a vector of 3D grid points bounded by a rectangle, defined by its
;;    opposite corner coordinates [`x1`, `y1`, 0] and [`x2`, `y2`, 0]. The grid
;;    points will be spaced along the x and y axes at the distance given by
;;    `resolution`. All z coordinates are initialized to nil."
;;   [x1 y1 x2 y2 resolution]
;;   (let [xs (range x1 x2 resolution)
;;         ys (range y1 y2 resolution)]
;;     (vec (for [x xs
;;                y ys]
;;            [x y nil]))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Flat grid functions ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn point-grid-flat
  "Return a flat vector of dimensions `n` by `m` with floats initialized to
   Float/NaN"
  [n m]
  ;; Use vector-of to get unboxed values for space and time efficiency
  (apply vector-of :float (repeat (* n m) Float/NaN)))

(comment
  (count (point-grid-flat 8 8))
  (Float/NaN)
  )

(defn get-idx-flat
  [n x y]
  (+ x (* n y)))

(defn get-val-flat
  "For n * m matrix (x, y) -> Return i = x + (n * y)"
  [grid n x y]
  (nth grid (get-idx-flat n x y)))

(defn set-val-flat
  "Sets the value `val` at position `x`, `y` in the `n`-by-m matrix `grid`
   implemented as a flat vector"
  [grid n x y val]
  {:pre [(have? #(< % n) x)]}
  (let [idx (have #(< % (count grid)) (get-idx-flat n x y))]
    (assoc grid idx val)))

(defn copy-data-to-grid
  "Copy data points from nested vector `data` (e.g. [[x1 y1 z1] [x2 y2 z2] ...])
   to flat, n-by-m vector `grid` (e.g. [z1 z2 z3 ...])"
  [grid n data]

  ; Unpack first row values
  (let [[x y z] (first data)]

    ; Check for all rows exhausted
    (if (empty? data)

      ; Return the resultant grid
      grid

      ; Else recur (call the loop with new values)
      (recur (set-val-flat grid n x y z) n (rest data)))))

(comment
  (do
    (def src-data [[0 1 3] [1 0 6] [1 2 9]])
    (def dst-grid (point-grid-flat 3 3))

    (copy-data-to-grid dst-grid 3 src-data))

  (empty? (rest [[0 1 2]]))
  )

(defn draw-grid-flat
  "Draw the grid using Quil library functions"
  [grid n z-min z-max]
  (println "z-min:" z-min)
  (println "z-max:" z-max)
  (doseq [x (range n)
          y (range (/ (count grid) n))
          :let [v (get-val-flat grid n x y)
                color (z-height->hue-rainbow v z-min z-max)]]
    (if (= color (:black colors))
      (q/stroke-weight 0)
      (q/stroke-weight 5))
    (apply q/stroke color)
    (q/point x y)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def ALL-X (sp/path sp/ALL sp/FIRST))
(def ALL-Y (sp/path sp/ALL sp/nthpath 1))
(def ALL-Z (sp/path sp/ALL sp/LAST))

(defn get-stat [stat path points] (apply stat (sp/select [path] points)))

(def get-min (partial get-stat min))
(def get-max (partial get-stat max))


(defn project-WGS84->meters
  "Project WGS84 data input as vector tuples for each point like:
   [lat-degrees lon-degrees alt-meters] and transform them to offsets in meters
   relative to the west-most and north-most coordinates in the data, which will
   be set to [0 0]. Altitudes, which are assumed to be in meters already, will
   be unchanged."
  [points]
  (let [lat-min (get-min ALL-X points)
        lat-max (get-max ALL-X points)
        lon-min (get-min ALL-Y points)
        lon-max (get-max ALL-Y points)
        ;; Get meters per degree of lat and lon for the center of the data
        mid-m-per-deg-lat (m-per-deg-lat (/ (+ lat-min lat-max) 2))
        mid-m-per-deg-lon (m-per-deg-lon (/ (+ lon-min lon-max) 2))]
    (map (fn scale-data [[lat lon alt]]
           [(* mid-m-per-deg-lat (- lat lat-min))
            (* mid-m-per-deg-lon (- lon lon-min))
            alt])
         points)))


(defn scale-map-data
  "Scale points to fit within the sketch window."
  [x-max y-max points]
  (let [lat-min (get-min ALL-X points)
        lat-max (get-max ALL-X points)
        lon-min (get-min ALL-Y points)
        lon-max (get-max ALL-Y points)]
    (map (fn [[lat lon alt]]
           ;; Map lat and lon ranges in reverse because N latitude
           ;; and W longitude increase in the directions opposite
           ;; of the Quil sketch window. Also, map lat -> y and lon -> x.
           [(q/map-range lon lon-min lon-max x-max 0)
            (q/map-range lat lat-min lat-max y-max 0)
            alt])
         points)))

;; (defn dist-1d [a b] (Math/abs (- a b)))

;; (defn get-nearest-points-goofy
;;   [[x y _] map-data]
;;   (let [closest-x (->> map-data
;;                        (sort-by first #(< (dist-1d x %1) (dist-1d x %2)))
;;                        (take 3)
;;                        set)] ; Set of points with closest x coord
;;     (lazy-seq (->> map-data
;;                    (sort-by second #(< (dist-1d y %1) (dist-1d y %2)))
;;                    (take 3)
;;                    (into closest-x))))) ; Combined with set of closest y coords

;; (defn get-nearest-points-easy
;;   "For a given arbitrary point described by `x` `y` in a list or vector,
;;    get the 4 nearest points found in `map-data`"
;;   [[x y] map-data]
;;   (lazy-seq (->> map-data
;;                  (sort-by #(q/dist x y (first %) (second %)))
;;                  (take 4))))

;; (defn point-within-square-dist?
;;   "Return true if the two given points are within the square defined by length
;;    `dist`"
;;   [[x1 y1] [x2 y2] dist]
;;   (and (< (dist-1d x1 x2) dist)
;;        (< (dist-1d y1 y2) dist)))

;; (defn get-nearest-points-slower
;;   "For a given arbitrary point described by `x` `y` in a list or vector,
;;    get at minimum the nearest 3 points found in `map-data`"
;;   ([point map-data] (get-nearest-points-slower point map-data 10))
;;   ([point map-data dist]
;;    (let [points (filter #(point-within-square-dist? point % dist) map-data)]
;;      (if (> (count points) 3)
;;        points
;;        (recur point map-data (+ 10 dist))))))

(defn round-row [[x y z]] [(m/round x) (m/round y) z])

(defn round-data-points
  "Run a rounding function on each row of the 2D list supplied in `coll`"
  [coll] (map round-row coll))

;; (defn interpolate-distance)

;; (defn get-nearest-points-grid)


;; (defn interpolate-altitude
;;   "Given a list/vector of `nearest-points` with altitudes, interpolate
;;    the altitude for the given `point`. Returns a single altitude value."
;;   [point nearest-points]
;;   (let [[x y]       point
;;         dists       (map (fn [[xn yn]]
;;                            (q/dist x y xn yn))
;;                          nearest-points)
;;         sum-dists   (apply + dists)
;;         ;; The start and stop values are swapped for getting normalized
;;         ;; distances so that closer points are weighted more heavily.
;;         weights     (map #(q/norm % sum-dists 0) dists)
;;         sum-weights (apply + weights)]
;;     #_(println "dists:" dists "sum:" sum-dists "\nweights:" weights "sum:" sum-weights)
;;     ;; Weighted mean
;;     (/ (apply + (map (fn [weight altitude] (* weight altitude))
;;                      weights
;;                      (sp/select [ALL-Z] nearest-points)))
;;        sum-weights)))

;; (defn interp-grid
;;   "Use the points from the `data` coll to interpolate altitudes for the `grid`
;;    points."
;;   [grid data]
;;   (let [near-points (for [point grid]
;;                       (get-nearest-points-easy point data))]
;;     (map (fn [[x y _ :as point] coll]
;;            [x y (interpolate-altitude point coll)])
;;          grid
;;          near-points)))



(defn get-data-proportion
  "Width/height"
  [map-data]
  (/ (- (get-max ALL-X map-data) (get-min ALL-X map-data))
     (- (get-max ALL-Y map-data) (get-min ALL-Y map-data))))



;;; Import CSV data
(def data (load-csv "/Users/easy/Downloads/20220519082008-04213-data.csv"))

(def imported-meters-data (->> data
                               clean-csv-data
                               drop-header-row
                               csv-strings->numbers
                               project-WGS84->meters))
(println "Imported point count:\n" (count imported-meters-data))

;;; Sketch parameters
(def swidth 700)
;; Scale the sketch according to the proportions of the map data
(def proportion (get-data-proportion imported-meters-data))
(def sheight (m/round (* swidth proportion)))

;; Set window parameters with padding so points are not drawn along the edges
(def spad 10)
(def pwidth (- swidth spad spad))
(def pheight (- sheight spad spad))



;; ;; Number of grid pixels to interpolate/render across each dimension
;; (def px-count 100)
;; ;; Resulting size of each grid pixel
;; (def px-width (double (/ (- window-x-max window-x-min) px-count)))
;; (def px-height (double (/ (- window-y-max window-y-min) px-count)))

;; Scale the map data
(def scaled-data
  (round-data-points
   (scale-map-data (dec pwidth) (dec pheight) imported-meters-data)))
(println "Scaled point count:\n" (count scaled-data))

(def z-min (get-min ALL-Z scaled-data))
(def z-max (get-max ALL-Z scaled-data))

;; (def grid-resolution 4)
;; (def scaled-grid (point-grid window-x-min window-y-min
;;                                window-x-max window-y-max
;;                                grid-resolution))
;; ;; Set the size of the square box within which interp-grid will search for
;; ;; neighboring points
;; (def interpolated-grid (interp-grid scaled-grid scaled-data))


;; Create a flat, 2D vector with all values initialized to nil
(def empty-grid
  (point-grid-flat pwidth pheight))
(println "Total point count:\n" (count empty-grid))

(have? #(every? number? %) empty-grid)
(take 5 empty-grid)

(comment
  (have? #(every? number? %) [2 3 4])
  (have? #(every? number? %) [2 3 4 [2 3]])
  )

;; X dimension of 2D vector
(def n-dim pwidth)

;; Copy the sparse grid data into the empty grid
(def data-grid
  (copy-data-to-grid empty-grid n-dim scaled-data))
(println "Non-zero point count:\n" (count (filter #(not (zero? %)) data-grid)))

(have? #(every? number? %) data-grid)

;; (prn data-grid)







(defn setup []
  (q/color-mode :hsb 360 100 100 1.0))

(defn settings []
  (q/pixel-density 2))

(defn draw []
  (q/background 0)

  (q/rect-mode :center)
  (q/translate spad spad)
  (draw-grid-flat data-grid n-dim z-min z-max)

  ;; (q/stroke-weight 5)
  ;; (apply q/stroke (z-height->hue-rainbow 265 z-min z-max))
  ;; (q/point 100 100)

  (q/no-loop))

(q/defsketch grid-thing
  :size [swidth sheight]
  ;; :renderer :p3d
  :settings settings
  :setup setup
  :draw draw
  :features [:keep-on-top])





;; (defn -main
;;   "Read a CSV, massage the data, interpolate some points, and plot the data"
;;   [& args]
;;   (println "Hello, World!"))


