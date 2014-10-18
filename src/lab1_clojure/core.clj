(ns lab1-clojure.core
  (:gen-class))

;;debugging parts of expressions
(defmacro dbg[x] `(let [x# ~x] (println "dbg:" '~x "=" x#) x#))

(def r_a 3.0)
(def r_b (* 1.5 r_a))
(def e_max 0.5)
(def e_min 0.15)

(def pot_a_koeff (/ 4 (Math/pow r_a 2)))
(def pot_b_koeff (/ 4 (Math/pow r_b 2)))

(defn euclidean-distance [coords1 coords2]
  {:pre [(= (count coords1) (count coords2))]}
  (Math/sqrt (apply + (map #(Math/pow (- %1 %2) 2) coords1 coords2))))

(defn hamming-distance [coords1 coords2]
  {:pre [(= (count coords1) (count coords2))]}
  (apply + (map #(if (= %1 %2) 0 1) coords1 coords2)))

(defn calculate-potential [point, points, distance-func]
  (let [new-potential
        (reduce
          (fn [res other-point]
          (+
            res
            (Math/pow
              Math/E
              (*
                (- pot_a_koeff)
                (distance-func
                  (point :coords)
                  (other-point :coords))))))
            0.0
            points)]

    (assoc point :potential new-potential)))

(defn calculate-potentials [points distance-func]
  (map #(calculate-potential % points distance-func) points))

(defn revise-potential [point points base-point distance-func]
  (let [revised-potential
        (-
          (point :potential)
          (*
            (base-point :potential)
            (Math/pow
              Math/E
              (*
                (- pot_b_koeff)
                (distance-func
                  (point :coords)
                  (base-point :coords))))))]

    (assoc point :potential revised-potential)))

(defn revise-potentials [points base-point distance-func]
  (map #(revise-potential % points base-point distance-func) points))

(defn parse-line-to-point [line]
  {:coords (vec (map #(Double/parseDouble %) (drop-last (.split #"," line))))
   :potential 0.0})

(defn max-potential-point [points]
  (apply max-key
    (fn [point] (point :potential))
    points))

(defn nullify-point-potential [points target-point]
  (map
    (fn [point]
      (if
        (=
          (point :coords)
          (target-point :coords))
        (assoc point :potential 0.0)
        point))
    points))

(defn shortest-distance [point points distance-func]
  (Math/sqrt (apply min (map #(distance-func (point :coords) (% :coords)) points))))

(defn find-centers
  ([points distance-func]
    (find-centers points nil [] distance-func))

  ([points first-center-potential centers distance-func]
    (let [max-point (max-potential-point points)
         revised-points (revise-potentials points max-point distance-func)]

      (if-not first-center-potential
        (recur revised-points (max-point :potential) [max-point] distance-func)
        (if
          (>
            (max-point :potential)
            (* e_max first-center-potential))
          (recur revised-points first-center-potential (conj centers max-point) distance-func)
          (if
            (<
              (max-point :potential)
              (* e_min first-center-potential))
            centers
            (if
              (>=
                (+
                  (/ (shortest-distance max-point centers distance-func) r_a)
                  (/ (max-point :potential) first-center-potential))
                1)
              (recur revised-points first-center-potential (conj centers max-point) distance-func)
              (recur
                (nullify-point-potential revised-points max-point)
                first-center-potential
                centers
                distance-func))))))))

(defn read-points-from-file [path]
  {:pre [(not (nil? path))]}
  (with-open [r (clojure.java.io/reader path)]
    (doall (map #(parse-line-to-point %)
      (line-seq r)))))

(defn -main
  [distance-type file]
  (let [distance-func (case distance-type
                        "euclidean" euclidean-distance
                        "hamming" hamming-distance)
        points (read-points-from-file file)
        points (calculate-potentials points distance-func)]
    (doseq [center (find-centers points distance-func)] (println center))))
