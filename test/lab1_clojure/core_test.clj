(ns lab1-clojure.core-test
  (:require [clojure.test :refer :all]
            [lab1-clojure.core :refer :all]))

(deftest test-euclidean-distance
  (is (=
    (euclidean-distance [10 27] [5 15])
    13.0))

  (is (thrown? AssertionError (euclidean-distance [5 2] [6 4 28]))))

(deftest test-hamming-distance
  (is (=
    (hamming-distance [2 1 7 3 8 9 6] [2 2 3 3 7 9 6])
    3))

  (is (thrown? AssertionError (hamming-distance [5 2] [6 4 28]))))

(deftest test-calculate-potential
  (let [point1 {:coords [0.0 3.0], :potential 0.0}
        point2 {:coords [1.0 5.0], :potential 0.0}
        distance-func euclidean-distance]

    (is (=
      (-> (calculate-potential point2 [point1 point2] distance-func) :potential)
      1.3701644213593966))))

(deftest test-parse-line-to-point
  (is (=
    (parse-line-to-point "48,12,5.4,Iris-setosa")
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

(deftest test-nullify-point-potential
  (is (=
    (nullify-point-potential
      [{:coords [5.0 8.0], :potential 3.0} {:coords [6.0 1.0], :potential 5.0}]
      {:coords [6.0 1.0], :potential 5.0})
    [{:coords [5.0 8.0], :potential 3.0} {:coords [6.0 1.0], :potential 0.0}])))
