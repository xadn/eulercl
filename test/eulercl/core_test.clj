(ns eulercl.core-test
  (:require [clojure.test :refer :all]
            [eulercl.core :refer :all])
  (:import [eulercl.core FactorPower]))

(deftest least-common-multiple-test
  (testing "easy least-common-multiple"
    (is (= (least-common-multiple '(8 9 21)) 504)))
  (testing "medium least-common-multiple"
    (is (= (least-common-multiple (range 1 11)) 2520)))
  (testing "euler least-common-multiple"
    (is (= (least-common-multiple (range 1 21)) 232792560))))

(deftest factor-powers-of-test
    (testing "grouping factor powers"
      (is (= (factor-powers-of '(2 2 2 3))
             [(FactorPower. 2 3) (FactorPower. 3 1)]))))

(deftest sum-of-squares-test (testing "sum of squares"
  (is (= (sum-of-squares (range 1 11)) 385))))

(deftest square-of-sums-test (testing "square of sums"
  (is(= (square-of-sums (range 1 11)) 3025))))

(deftest problem-6-test
  (is (=  (problem-6 (range 1 11)) 2640))
  (is (=  (problem-6) 25164150)))

