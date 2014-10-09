(ns lab1-clojure.core-test
  (:require [clojure.test :refer :all]
            [lab1-clojure.core :refer :all]))

(deftest test-square-dist
  (is (=
    (square-dist [10 20] [5 15])
    50.0))

  (is (=
    (square-dist [10 20 7] [5 15 14])
    99.0))

  (is (thrown? AssertionError (square-dist [5 2] [6 4 28]))))

(deftest test-calculate-potential
  (let [point1 {:coords [0.0 3.0], :potential 0.0}
        point2 {:coords [1.0 5.0], :potential 0.0}]

    (is (=
      (-> (calculate-potential point2 [point1 point2]) :potential)
      1.108368023221896))))

(deftest test-parse-line-to-point
  (is (=
    (parse-line-to-point "48,12,5.4")
    {:coords [48.0 12.0 5.4], :potential 0.0})))

(deftest test-read-points-from-file
  (let [path "test/fixtures/points.txt"
        points [{:coords [5.0 8.0], :potential 0.0} {:coords [6.0 1.0], :potential 0.0}]]
    (is (=
      (read-points-from-file path)
      points)))

  (is (thrown? AssertionError (read-points-from-file nil)))

  (is (thrown?
    java.io.FileNotFoundException
    (read-points-from-file "non-existing-file.txt"))))
