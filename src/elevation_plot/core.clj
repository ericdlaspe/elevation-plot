(ns elevation-plot.core
  (:gen-class)
  (:require
   [taoensso.truss :as truss :refer (have have?)]
   [clojure.data.csv :as csv]
   [clojure.java.io :as io]
   [clojure.math.numeric-tower :as m]
   [clojure.tools.trace :as tr]
   [com.rpl.specter :as sp]
   [quil.core :as q]
   ))

(def csv-path "/Users/easy/Downloads/20220519082008-04213-data.csv")

(def width 700)
(def height width)
(def pad 10)
;;; Use an atom to pass final data from main to draw.
(def drawing-state (atom {}))


(defn isNaN?
  "Returns true if `x` is ##NaN. Else, false."
  [x]
  (Float/isNaN x))

(defn not-NaN
  "Returns `x` if `x` is not ##NaN, otherwise returns false"
  [x]
  (if (Float/isNaN x) false x))


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
    (:black colors)))  ; z is outside the expected data range

(comment
  (< 0 nil 5)
  ;; => Execution error (NullPointerException) at elevation-plot.core/eval12443 (form-init1909576536610475304.clj:37).
  ;;    null
  )

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
   `fill`, or if no fill value is given, Float/NaN.

   Example:
   ```clojure
     (point-grid-flat 3 2)
     ;; => [##NaN ##NaN ##NaN ##NaN ##NaN ##NaN]
   ```
   Note that these ##NaN values cannot "
  ([n m]
   (point-grid-flat n m Float/NaN))
  ([n m fill]
  ;; Use vector-of to get unboxed values for space and time efficiency
   (apply vector-of :float (repeat (* n m) fill))))

(comment
  (point-grid-flat 3 2)
  ;; => [##NaN ##NaN ##NaN ##NaN ##NaN ##NaN]

  (apply vector-of :float (repeat (* 3 3) nil))
  ;; => Execution error (NullPointerException) at elevation-plot.core/eval12744 (form-init1909576536610475304.clj:140).
  ;;    null

)

(defn get-idx-flat
  [n x y]
  (+ x (* n y)))

(defn get-val-flat
  "For n * m matrix, `grid`. Return grid[i], where i = x + (n * y)"
  [grid n x y]
  (get grid (get-idx-flat n x y)))

(defn set-val-flat
  "Sets the value `val` at position `x`, `y` in the `n`-by-m matrix `grid`
   implemented as a flat vector"
  [grid n x y val]
  (have? #(< % n) x)
  (let [idx (have #(< % (count grid)) (get-idx-flat n x y))]
    (assoc grid idx val)))

(defn copy-data-to-grid
  "Copy data points from nested vector `data` (e.g. [[x1 y1 z1] [x2 y2 z2] ...])
   to flat, n-by-m vector `grid` (e.g. [z1 z2 z3 ...])"
  [grid n data]
  ;;; FIXME: Sort data by x and y vals before copying into flat grid

  ; Unpack first row values
  (let [[x y z] (first data)]

    ; Check for all rows exhausted
    (if (empty? data)

      ; Return the resultant grid
      grid

      ; Else recur
      (recur (set-val-flat grid n x y z) n (rest data)))))

(comment
  (do
    (def src-data [[0 1 3] [1 0 6] [1 2 9]])
    (def dst-grid (point-grid-flat 3 3))

    (copy-data-to-grid dst-grid 3 src-data))

  (empty? (rest [[0 1 2]])))

(defn draw-grid-flat
  "Draw the n-by-m `grid` using Quil library functions. m does not need to be
   supplied. It is implied to be the vector's size divided by `n`."
  [grid n z-min z-max]
  (have? pos? n)
  (q/stroke-weight 4)
  (doseq [x (range n)
          y (range (/ (count grid) n))
          :let [v (have number? (get-val-flat grid n x y))
                color (z-height->hue-rainbow v z-min z-max)]]
    (apply q/stroke color)
    (when-not (= color (:black colors))
      (q/point x y))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def ALL-X (sp/path sp/ALL sp/FIRST))
(def ALL-Y (sp/path sp/ALL sp/nthpath 1))
(def ALL-Z (sp/path sp/ALL sp/LAST))

(defn get-stat [stat path points] (apply stat (sp/select [path] points)))

(def get-min (partial get-stat min))
(def get-max (partial get-stat max))


(defn project-wgs84->meters
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
        ;; Get meters per degree of lat and lon for the center of the data.
        ;; For a mapped area of a few miles or less, this should provide
        ;; a close enough approximation. If greater accuracy is needed,
        ;; we could later switch to calculating the m-per-deg-lat between each
        ;; two points.
        mid-m-per-deg-lat (m-per-deg-lat (/ (+ lat-min lat-max) 2))
        mid-m-per-deg-lon (m-per-deg-lon (/ (+ lon-min lon-max) 2))]
    ;; FIXME: Consider a more memory-efficient way of building this vector. Reduce?
    ;; FIXME: Build a vector instead of a list.
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
           [(q/map-range lon lon-min lon-max (dec x-max) 0)
            (q/map-range lat lat-min lat-max (dec y-max) 0)
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
  [coll] (mapv round-row coll))


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

(defn get-sketch-dimensions
  "Scale the sketch parameters according to the proportions of the map data."
  [meters-data]
  ;; Set window parameters with padding so points are not drawn along the edges
  (let [proportion (get-data-proportion meters-data)
        pwidth (- width (* 2 pad))
        pheight (- (m/round (* width proportion))
                   (* 2 pad))]

    ;; Check that the sketch proportions will fit within the window
    (have? (and (> width pwidth) (> height pheight)))

    ;; Return the sketch proportions
    {:pwidth pwidth
     :pheight pheight}))

(defn get-altitude-extents
  "Return the minimum and maximum altitude in the vector, which may contain 
   ##NaN values."
  [coll]
  (let [data (filter #(not (isNaN? %)) coll)]
    {:z-min (apply min data)
     :z-max (apply max data)}))


(defn import-wgs84-data
  "Import WGS84 GIS data points from a CSV with rows formatted as
   latitude, longitude, altitude (m)
   where latitude and longitude are in decimal degrees.
   The output is a vector of vectors, (e.g.
   [39.661031139,-84.025190726,276.5])"
  [file-path]
  (->> (load-csv file-path)
       clean-csv-data
       drop-header-row
       csv-strings->numbers))

(defn in-grid?
  "Is the point described by `x` and `y` inside an n-by-m grid?"
  [n m x y]
  (and (<= 0 x (dec n))
       (<= 0 y (dec m))))

(comment
  (let [[n m x y] [3 4 1 1]]
    (>= 0 x (dec n))
    (>= 0 y (dec m)))
  
  (>= 0 0 (dec 3))
  (>= 0 1)
  )

(defn get-altitude
  [grid n m x y]
  (or (and
       ;; false if outside n-by-m grid
       (in-grid? n m x y)
       ;; nil if index does not exist
       (get-val-flat grid n x y))
      Float/NaN))

(defn idx->xy
  [i n]
  (have? pos? n)
  [(mod i n) (quot i n)])


(defn get-subgrid
  "Return the 3-by-3 subgrid of altitudes centered on index `i`. Fills in ##NaN
   at points where the subgrid overlaps with the edges."
  ([grid n i]
   (apply get-subgrid grid n (idx->xy i n)))

  ([grid n x y]
   (have? pos? n)
   (let [m (/ (count grid) n)]

     (vec (for [ax [(dec x) x (inc x)]
                ay [(dec y) y (inc y)]]
            (get-altitude grid n m ax ay))))))

(comment
  (let [x 5 y 2 n 8]
    (for [x [(dec x) x (inc x)]
          y [(dec y) y (inc y)]]
      (get-idx-flat n x y)))
  ;; => (12 20 28 13 21 29 14 22 30)

  (get-subgrid [0 1 2 3 4 5 6 7 8 9 10 11] 3 0 1)
  ;; => [##NaN ##NaN ##NaN 0 3 6 1 4 7]

  (get-subgrid [0 1 2 3 4 5 6 7 8 9] 3 0 0)
  ;; => [##NaN ##NaN ##NaN ##NaN 0 3 ##NaN 1 4]
  )

(def counts (partial map count))

(defn counts-equal?
  "Return true if the number of items in all collections inside `coll` is the
   same. Otherwise, false."
  [coll]
  (apply = (counts coll)))

(defn weighted-mean
  "Return the weighted mean of `terms` weighted by `weights`.
   Weights should have a 1:1 correspondence with terms (i.e., the sequences must
   be the same length). The `weights` need not add up to 1."
  [terms weights]
  ;; Some assertions
  (have? not-empty terms)
  (have? counts-equal? [terms weights])
  (let [sum-weights (reduce + weights)
        sum-weighted-terms (reduce + (map * weights terms))]
    (have? pos? sum-weights)
    (/ sum-weighted-terms sum-weights)))

(comment
  (do
    (point-grid-flat 3 3)
    (have pos? 0)
    (println "foo"))

  (= (count []) (count (not-empty [])))
  (counted? [1 2 3])
  
  (map count [[1 2 3] [4 5] [0 0 0 0 0]])
  (fn [coll] (map count coll) [[1 2 3] [4 5]])
  (apply = [1 1 1])

  (counts-equal? [[1 2 3] [4 5 4]])
  (counts-equal? [[1 2 3] [4 5]])

  (apply = (counts [[1 2 3] [4 5 4]]))
  )

(defn count-not-NaN
  [coll]
  (count (filter not-NaN coll)))

(defn interpolate-subgrid-val
  "From the 3-by-3 flat vector `subgrid` interpolate the value at the center.
   If the value is already not ##NaN, return it as-is."
  [subgrid]
  (let [center (nth subgrid 4)]
    ;; Return the value at the center of the subgrid if it already has a value
    ;; or if we don't have enough surrounding values from which to interpolate.
    (if (or (not-NaN center)
            (> 3 (count-not-NaN subgrid)))
      center

    ;; Compute a weighted mean of the 8 values surrounding the center.
      (let [corners  (->> subgrid
                        ;; The center has an even index as well, but it will
                        ;; always be filtered out becuase it is ##NaN.
                          (keep-indexed (fn [idx val] (when (even? idx) val)))
                          (filter not-NaN))
            laterals (->> subgrid
                          (keep-indexed (fn [idx val] (when (odd? idx) val)))
                          (filter not-NaN))
        ;; Map the weights of corner values in proportion with their distance
        ;; compared to the lateral values.
        ;; 0.70711356 is approximately the inverse of the length of the
        ;; hypotenuse of a unit right triangle.
            corner-weights  (map (constantly 0.70711356) corners)
            lateral-weights (map (constantly 1.0) laterals)

            values (concat corners laterals)
            weights (concat corner-weights lateral-weights)]
        (weighted-mean values weights)))))


(comment
  (keep-indexed (fn [idx val] (when (even? idx) val)) [10 20 30 40 50 60 70])
  ;; => (10 30 50 70)

  (/ 1 1.4142)
  ;; => 0.7071135624381276

  (map (constantly 1) [0 1 2 3])
  ;; => (1 1 1 1)

  (float 0.7071135624381276)

  (when 5.7 (println "true"))

  (macroexpand (when (or (not-NaN ##NaN)
                         (> 3 (count-not-NaN [1 2 3 4 5 6 7 8])))
                 4))
  )


(defn interpolate-linear-8-way
  "Interpolate each value in a flat 2D vector, based on at least 3 and up to
   8 adjacent values. Missing values (represented by ##NaN) are ignored."
  [grid n]
  (have? pos? n)
  (let [size (count grid)]
    (loop [idx 0
           work-grid grid]

      ;; When there are no ##NaN left in the grid, return it as complete.
      (when (not-any? isNaN? work-grid)
        work-grid)

      (if (>= idx size)
        ;; When we reach the last element in the grid, reset the index to 0
        ;; and interpolate over the whole grid again.
        (recur 0 work-grid)

        ;; This is where we do the work of interpolating each point that doesn't
        ;; already have a value.
        (if (isNaN? (nth work-grid idx))
          (let [val (interpolate-subgrid-val (get-subgrid work-grid n idx))]
            (recur (inc idx) (assoc work-grid idx val)))

          ;; The grid point at idx already has a value. Move along...
          (recur (inc idx) work-grid))))))


(defn setup []
  (q/color-mode :hsb 360 100 100 1.0))

(defn settings []
  (q/pixel-density 2)
  (q/smooth))

(defn draw []
  (apply q/background (:black colors))

  (let [{data-grid :data-grid, n-dim :pwidth,
         z-min :z-min, z-max :z-max} @drawing-state]
    (q/translate pad pad)
    (draw-grid-flat data-grid n-dim z-min z-max))

  (q/no-loop))



(comment
  (do
    (def imported-meters-data (->> (import-wgs84-data csv-path)
                                   project-wgs84->meters))
    (def pwidth (:pwidth (get-sketch-dimensions imported-meters-data)))
    (def pheight (:pheight (get-sketch-dimensions imported-meters-data)))
    (def scaled-meters-data (->> imported-meters-data
                                 (scale-map-data pwidth pheight)
                                 round-data-points))
    (count scaled-meters-data)

    (def grid (point-grid-flat pwidth pheight))
    (count grid)
    (def data-grid (copy-data-to-grid grid pwidth scaled-meters-data))
    (filter #(not (Float/isNaN %)) data-grid)
    (def extents (get-altitude-extents data-grid)))

  extents)


(defn -main
  "Read a CSV, massage the data, interpolate some points, and plot the data"
  ;; [& args]
  []
  ;; Parse program arguments

  ;; Read the CSV. Clean, project, scale, and round the data to within 1 m.
  (let [imported-meters-data (->> (import-wgs84-data csv-path)
                                  project-wgs84->meters)
        {:keys [pwidth pheight]} (get-sketch-dimensions imported-meters-data)
        scaled-meters-data (->> imported-meters-data
                                (scale-map-data pwidth pheight)
                                round-data-points)

        ;; Create a flat, 2D vector with all values initialized to NaN.
        ;; Then, copy our scaled map data into the vector.
        data-grid (-> (point-grid-flat pwidth pheight)
                      (copy-data-to-grid pwidth scaled-meters-data)
                      #_(interpolate-linear-8-way pwidth))

        {:keys [z-min z-max]} (get-altitude-extents data-grid)]

    (reset! drawing-state {:data-grid data-grid
                           :pwidth pwidth
                           :z-min z-min
                           :z-max z-max})

    (q/defsketch grid-thing
      :size [width height]
      :settings settings
      :setup setup
      :draw draw
      :features [:keep-on-top])))

(-main)


