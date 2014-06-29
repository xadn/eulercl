(ns eulercl.core-test
  (:require [clojure.test :refer :all]
            [eulercl.core :refer :all]))

(deftest least-common-multiple-test
  (testing "least-common-multiple"
    (is (= (least-common-multiple '(8 9 21)) 504))))