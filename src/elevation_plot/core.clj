(ns elevation-plot.core
  (:gen-class)
  (:require [taoensso.truss :as truss :refer (have have! have?)]
            [clojure.data.csv :as csv]
            [clojure.java.io :as io]
            ;; [clojure.math.numeric-tower :as m]
            [clojure.pprint :as pp]
            [quil.core :as q]
            ;; [quil.middleware :as qm]
            [com.rpl.specter :as sp]))


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
       (map strings->doubles)
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

(defn point-grid
  "Return a vector of 3D grid points bounded by a rectangle, defined by its
   opposite corner coordinates [`x1`, `y1`, 0] and [`x2`, `y2`, 0]. The grid
   points will be spaced along the x and y axes at the distance given by
   `resolution`. All z coordinates are initialized to 0."
  [x1 y1 x2 y2 resolution]
  (let [xs (range x1 x2 resolution)
        ys (range y1 y2 resolution)]
    (vec (for [x xs
               y ys]
           [x y 0]))))

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
    (map (fn [[lat lon alt]]
           [(* mid-m-per-deg-lat (- lat lat-min))
            (* mid-m-per-deg-lon (- lon lon-min))
            alt])
         points)))


(defn scale-map-data
  "Scale points to fit within the sketch window."
  [window-x-min window-x-max window-y-min window-y-max points]
  (let [lat-min (get-min ALL-X points)
        lat-max (get-max ALL-X points)
        lon-min (get-min ALL-Y points)
        lon-max (get-max ALL-Y points)]
    (map (fn [[lat lon alt]]
           ;; Map lat and lon ranges in reverse because N latitude
           ;; and W longitude increase in the directions opposite
           ;; of the Quil sketch window. Also, map lat -> y and lon -> x.
           [(q/map-range lon lon-min lon-max window-x-max window-x-min)
            (q/map-range lat lat-min lat-max window-y-max window-y-min)
            alt])
         points)))

(defn dist-1d [a b] (Math/abs (- a b)))

(defn get-nearest-points-goofy
  [[x y _] map-data]
  (let [closest-x (->> map-data
                       (sort-by #(dist-1d x ))
                       (take 3)
                       set) ; Set of points with closest x coord
        closest (->> map-data
                     (sort-by second #(< (dist-1d y %1) (dist-1d y %2)))
                     (take 3)
                     (into closest-x))] ; Combined with set of closest y coord
    closest))

(defn get-nearest-points-easy
  "For a given arbitrary point described by `x` `y` in a list or vector,
   get the 4 nearest points found in `map-data`"
  [[x y] map-data]
  (->> map-data
       (sort-by #(q/dist x y (first %) (second %)))
       (take 4)))

(defn interpolate-point-alt
  "Given a list/vector of `nearest-points` with altitudes, interpolate
   the altitude for the given `point`. Returns a single altitude value."
  [point nearest-points]
  (let [[x y]       point
        dists       (map (fn [[xn yn]]
                           (q/dist x y xn yn))
                         nearest-points)
        sum-dists   (apply + dists)
        ;; The start and stop values are swapped for getting normalized
        ;; distances so that closer points are weighted more heavily.
        weights     (map #(q/norm % sum-dists 0) dists)
        sum-weights (apply + weights)]
    #_(println "dists:" dists "sum:" sum-dists "\nweights:" weights "sum:" sum-weights)
    ;; Weighted mean
    (/ (apply + (map (fn [weight altitude] (* weight altitude))
                     weights
                     (sp/select [ALL-Z] nearest-points)))
       sum-weights)))

(defn interp-grid
  [grid data]
  (let [near-points (for [point grid]
                      (get-nearest-points-easy point data))]
    (map (fn [[x y _ :as point] coll]
           [x y (interpolate-point-alt point coll)])
         grid
         near-points)))


(comment
  (q/dist 3 2 10 15)
  (q/dist 10 15 3 2))

(comment
  (do
    (def crud [[10 10 1]
               [10 -20 1]
               [10 30 1]
               [10 40 50]
               [10 50 30]
               [20 10 10]
               [19 29 0]
               [-2 20 0]
               [3 10 1]
               [50 10 5]
               [50 100 12]
               [50 30 20]
               [50 40 2]
               [50 50 1]])
    (reduce + (map first crud))
    (apply printf "min x: %d  max x: %d\nmin y: %d  max y: %d\nmin z: %d  max z: %d"
           ((juxt min-x max-x min-y max-y min-z max-z) crud))
    (println)
    (pp/pprint (scale-map-data crud))
    (get-nearest-points-goofy [22 30 0] crud)
    (def crap-grid (point-grid 10 10 100 100 20))
    (interp-grid crap-grid crud))
    )

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


;;; Sketch parameters
(def swidth 800)
;; Scale the sketch according to the proportions of the map data
(def proportion (get-data-proportion imported-meters-data))
(def sheight (* swidth proportion))

;; Set window parameters with padding so points are not drawn along the edges
(def spad 10)
(def window-x-min spad)
(def window-x-max (- swidth spad))
(def window-y-min spad)
(def window-y-max (- sheight spad))

;; Number of grid pixels to interpolate/render across each dimension
(def px-count 100)
;; Resulting size of each grid pixel
(def px-width (double (/ (- window-x-max window-x-min) px-count)))
(def px-height (double (/ (- window-y-max window-y-min) px-count)))


;; Scale the map data
(def scaled-data (scale-map-data window-x-min window-x-max window-y-min
                                 window-y-max imported-meters-data))

(def grid-resolution 5)
(def scaled-grid (point-grid window-x-min window-y-min
                             window-x-max window-y-max
                             grid-resolution))

;; Set the size of the square box within which interp-grid will search for
;; neighboring points
(def initial-distance)
(def interpolated-grid (interp-grid initial-distance scaled-grid scaled-data))


;;; Named colors in HSB( 360 100 100 1.0 )
(def colors {:black [0 0 0]
             :red [0 100 100]
             :blue [240 100 100]})

(defn get-hue
  [color]
  (first (color colors)))

(defn setup []
  (q/color-mode :hsb 360 100 100 1.0)
  (q/frame-rate 4))

(defn settings []
  (q/pixel-density 2))

(defn draw []
  (q/smooth 4)
  (q/background 0)
  (q/no-stroke)
  (q/rect-mode :center)
  (let [z-min (get-min [ALL-Z] scaled-data)
        z-max (get-max [ALL-Z] scaled-data)]
    (doall
     (map (fn [[x y z]]
            (q/fill (q/map-range z z-min z-max (get-hue :blue) (get-hue :red))
                    100 100)
            (q/rect x y px-width px-height))
          #_scaled-data
          interpolated-grid)))

  (q/no-loop))

(q/defsketch grid-thing
  :size [swidth sheight]
  ;; :renderer :p3d
  :settings settings
  :setup setup
  :draw draw
  :features [:keep-on-top])








(comment
  (doall
   (point-grid 10 10 51 51 10))
  )

(comment
  (degrees->meters 60.0)
  (degrees->meters 365.0)
  )


(comment

  (do
    (def sample-data '(["latitude" "longitude" "altitude (m)"]
                       ["39.661031139" "-84.025190726" "276.5"]
                       ["39.661035969" "" "276.5"]
                       ["39.661031139" "-84.025190726" "276.5" "5234.3"]
                       [""]
                       ["39.661107174" "-84.025124886"]
                       ["39.661190203" "-84.025273898" "276.6"]))
    ;; (def loaded-sample-data (load-csv "/Users/easy/Downloads/broken.csv"))
    ;; (pp/pprint sample-data)
    (def clean-data (->> data
                         clean-csv-data
                         drop-header-row
                         csv-strings->numbers))
    (let [norm-data (scale-map-data clean-data)]
      (pp/pprint (take 30 norm-data))
      (pp/pprint (get-min-z norm-data))))


  (let [[x1 y1] (first clean-data)
        [x2 y2] (second clean-data)]
    (prn x1 y1 x2 y2)
    (prn (degrees->meters (q/dist x1 y1 x2 y2))))
  )


;; (defn -main
;;   "Read a CSV, massage the data, interpolate some points, and plot the data"
;;   [& args]
;;   (println "Hello, World!"))


