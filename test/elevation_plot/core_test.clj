(ns elevation-plot.core-test
  (:require [clojure.test :refer :all]
            [elevation-plot.core :refer :all]))

(deftest copy-data-to-grid-test
  (testing "Example inputs"
    (let [(src-data [[0 1 3] [1 0 6] [1 2 9]])
          ; Destination grid is 3x3 flat vector
          (dst-grid [0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0])]
      (is (= (copy-data-to-grid dst-grid 3 src-data)
             [0.0 6.0 0.0 3.0 0.0 0.0 0.0 9.0 0.0])))))
