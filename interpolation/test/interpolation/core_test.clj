#_{:clj-kondo/ignore [:require-use]}
(ns interpolation.core-test
  (:require [clojure.test :refer [deftest is testing run-tests]]
            [interpolation.core :refer [linear-interpolation center-window lagrange-polynomial format-output lagrange-interpolation]]))

(deftest test-linear-interpolation
  (testing "Linear interpolation between two points"
    (let [points [[0.0 0.0] [1.0 1.0]]
          step 0.5
          expected [[0.0 0.0] [0.5 0.5] [1.0 1.0]]]
      (is (= expected (vec (linear-interpolation points step)))))))

(deftest test-center-window
  (testing "Testing center window"
    (let [points (vec (map vector (range 10) (range 10)))]
      (is (= (vec (center-window points 6))
             [[2 2]
              [3 3]
              [4 4]
              [5 5]
              [6 6]
              [7 7]])))
    (let [points (vec (map vector (range 10) (range 10)))]
      (is (= (vec (center-window points 2))
             [[4 4]
              [5 5]])))
    (let [points (vec (map vector (range 10) (range 10)))]
      (is (= (vec (center-window points 12))
             points)))))

(deftest test-lagrange-polynomial
  (let [points [[1 1] [2 4] [3 9]]]
    (is (= (lagrange-polynomial points 2) 4N))
    (is (= (lagrange-polynomial points 3) 9N))
    (is (= (lagrange-polynomial points 1) 1N))
    (is (= (lagrange-polynomial [[1 2]] 1) 2N))))

(deftest test-format-output
  (let [x-values [1.0 2.0 3.0]
        y-values [1.0 2.0 3.0]
        expected "1.00\t2.00\t3.00\n1.00\t2.00\t3.00\n"]
    (is (= (format-output x-values y-values) expected))))

(deftest test-lagrange-interpolation
  (let [points [[1 1] [2 4] [3 9]]
        step 0.5
        start-x 1
        end-x 3
        expected [[1.0 1.0]
                  [1.5 2.25]
                  [2.0 4.0]
                  [2.5 6.25]
                  [3.0 9.0]]]
    #_{:clj-kondo/ignore [:redundant-let]}
    (let [result (lagrange-interpolation points step start-x end-x)]
      (is (= (map #(map double %) result) expected)))))

(run-tests)
