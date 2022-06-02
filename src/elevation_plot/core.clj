(ns elevation-plot.core
  (:gen-class)
  (:require [taoensso.truss :as truss :refer (have have! have?)]
            [clojure.data.csv :as csv]
            [clojure.java.io :as io]
            [clojure.math.numeric-tower :as m]
            [clojure.pprint :as pp]
            [quil.core :as q]
            [quil.middleware :as qm]
            [com.rpl.specter :as sp]))

;;; Sketch parameters
(def sheight 800)
(def swidth 800)
(def spad 10)


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

(def m-per-deg-long
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
           (vector x y 0)))))

(def X-VALS (sp/path sp/ALL sp/FIRST))
(def Y-VALS (sp/path sp/ALL sp/nthpath 1))
(def Z-VALS (sp/path sp/ALL sp/LAST))

(defn get-min-x [points] (apply min (sp/select [X-VALS] points)))
(defn get-max-x [points] (apply max (sp/select [X-VALS] points)))
(defn get-min-y [points] (apply min (sp/select [Y-VALS] points)))
(defn get-max-y [points] (apply max (sp/select [Y-VALS] points)))
(defn get-min-z [points] (apply min (sp/select [Z-VALS] points)))
(defn get-max-z [points] (apply max (sp/select [Z-VALS] points)))

(def window-x-min spad)
(def window-x-max (- swidth spad))
(def window-y-min spad)
(def window-y-max (- sheight spad))

(defn scale-map-data
  "Scale points to fit within the sketch window."
  [points]
  (let [lat-min (get-min-x points)
        lat-max (get-max-x points)
        lon-min (get-min-y points)
        lon-max (get-max-y points)]
    (sp/transform [sp/ALL]
                  (fn [[lat lon alt]]
                    [(q/map-range lat lat-min lat-max window-x-min window-x-max)
                     (q/map-range lon lon-min lon-max window-y-min window-y-max)
                     alt])
                  points)))

(comment
  (def crud [[10 10 1]
             [10 -20 1]
             [10 30 1]
             [10 40 210]
             [10 50 1]
             [20 10 1]
             [-2 20 1]
             [3 10 1]
             [50 10 1]
             [50 100 1]
             [50 30 1]
             [50 40 1]
             [50 50 1]])
  (apply printf "min x: %d  max x: %d  min y: %d  max y: %d  min z: %d  max z: %d"
         ((juxt min-x max-x min-y max-y min-z max-z) crud))
  (pp/pprint (scale-map-data crud))
  )


;;; "Main"
(def data (load-csv "/Users/easy/Downloads/20220519082008-04213-data.csv"))

(def scaled-data (->> data
                      clean-csv-data
                      drop-header-row
                      csv-strings->numbers
                      scale-map-data))

(def grid-resolution 5)
(def scaled-grid (point-grid window-x-min window-y-min
                             window-x-max window-y-max
                             grid-resolution))

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
  (q/background 255)
  (q/stroke-weight 3)
  (let [z-min (get-min-z scaled-data)
        z-max (get-max-z scaled-data)]
    (doall
     (map (fn [[x y z]]
            (q/stroke (q/map-range z z-min z-max (get-hue :blue) (get-hue :red))
                      100 100)
            (q/point x y))
          scaled-data)))

  #_(apply q/stroke (:blue colors))
  #_(doall
   (map (fn [[x y]] (q/point x y))
        scaled-grid))

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


