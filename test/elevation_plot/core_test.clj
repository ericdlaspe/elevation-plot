(ns elevation-plot.core-test
  (:require [clojure.test :refer :all]
            [elevation-plot.core :refer :all]))

(deftest copy-data-to-grid-test
  (testing "Example inputs"
    (let [src-data [[0 1 3.0] [1 0 6.0] [1 2 9.0]]
          ; Destination grid is 3x3 flat vector
          dst-grid [0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0]]
      (is (= (copy-data-to-grid dst-grid 3 src-data)
             [0.0 6.0 0.0 3.0 0.0 0.0 0.0 9.0 0.0])))))

(deftest get-subgrid-test
  (testing "Example inputs"
    (is (compare (get-subgrid [0 1 2 3 4 5 6 7 8 9 10 11] 3 0 1)
                 [##NaN ##NaN ##NaN 0 3 6 1 4 7]))
    (is (compare (get-subgrid [0 1 2 3 4 5 6 7 8 9] 3 0 0)
                 [##NaN ##NaN ##NaN ##NaN 0 3 ##NaN 1 4]))))

(deftest counts-equal?-test
  (testing "Sanity checks"
    (is (true? (counts-equal? [[1 2 3] [4 5 6]])))
    (is (false? (counts-equal? [[1 2 3] [4 5]])))))

(deftest count-not-NaN-test
  (testing "Sanity checks and edge cases"
    (is (= (count-not-NaN [##NaN ##NaN ##NaN])
           0))
    (is (= (count-not-NaN [##NaN 1 ##NaN 2])
           2))
    (is (= (count-not-NaN [0])
           1))
    (is (= (count-not-NaN [])
           0))))

(deftest interpolate-subgrid-val-test
  (testing "A few typical cases"
    ;; 3-by-3 grid with n=3
    ;; ##NaN ##NaN ##NaN ##NaN ##NaN
    ;; ##NaN   0     1     2   ##NaN
    ;; ##NaN   3     4     5   ##NaN
    ;; ##NaN   6     7     8   ##NaN
    ;; ##NaN ##NaN ##NaN ##NaN ##NaN
    (is (= (interpolate-subgrid-val [##NaN 1 2 3 4 5 6 7 8] 3 0)
           ;; Contributing indices are 1 3 4, with 4 having lower weight
           2.5224114499282404))
    (is (= (interpolate-subgrid-val [0 1 2 3 ##NaN 5 6 7 8] 3 4)
           ;; I find this result unexpected and... juicy.
           4.0))
    (is (= (interpolate-subgrid-val [0 1 ##NaN 3 4 ##NaN 6 7 ##NaN] 3 5)
           4.0))
    ))
