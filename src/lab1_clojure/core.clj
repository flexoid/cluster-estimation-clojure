(ns lab1-clojure.core
  (:gen-class))

;;debugging parts of expressions
(defmacro dbg[x] `(let [x# ~x] (println "dbg:" '~x "=" x#) x#))

(def r_a 3.0)
(def r_b (* 1.5 r_a))
(def e_up 0.5)
(def e_down 0.15)

(def pot_a_koeff (/ 4 (Math/pow r_a 2)))
(def pot_b_koeff (/ 4 (Math/pow r_b 2)))

(defn square-dist [coords1 coords2]
  {:pre [(= (count coords1) (count coords2))]}
  (apply +
    (map
      (fn [x y] (Math/pow (- x y) 2))
      coords1
      coords2)))

(defn calculate-potential [point, points]
  (let [new-potential
         (reduce (fn [res other-point]
                 (+ res
                    (Math/pow Math/E
                              (* (- pot_a_koeff)
                                 (square-dist (-> point :coords)
                                              (-> other-point :coords))))))
                 0.0
                 points)]
    (assoc point :potential new-potential)))

(defn calculate-potentials [points]
  (map #(calculate-potential % points) points))

(defn revise-potential [point points base-point]
  (let [revised-potential
        (- (-> point :potential)
           (* (-> base-point :potential)
              (Math/pow Math/E
                        (* (- pot_b_koeff)
                           (square-dist (-> point :coords)
                                       (-> base-point :coords))))))]
    (assoc point :potential revised-potential)))

(defn revise-potentials [points base-point]
  (map #(revise-potential % points base-point) points))

(defn parse-line-to-point [line]
  {:coords (vec (map #(Double/parseDouble %) (.split #"," line)))
   :potential 0.0})

(defn max-potential-point [points]
  (apply max-key
         (fn [point] (-> point :potential))
         points))

(defn shortest-distance [point points]
  (Math/sqrt (apply min (map #(square-dist (-> point :coords) (-> % :coords)) points))))

(defn find-centers
  ([points]
    (find-centers points nil []))

  ([points first-center-potential centers]
    (let [max-point (max-potential-point points)
         revised-points (revise-potentials points max-point)]

      (if-not first-center-potential
          (recur revised-points (-> max-point :potential) [max-point])

          (if
            (or
              (>
                (-> max-point :potential)
                (* e_up first-center-potential))
              (>=
                (+
                  (/ (shortest-distance max-point centers) r_a)
                  (/ (-> max-point :potential) first-center-potential))
                1))
            (recur revised-points first-center-potential (conj centers max-point))
            centers)))))

(defn read-points-from-file [path]
  {:pre [(not (nil? path))]}
  (with-open [r (clojure.java.io/reader path)]
    (doall (map #(parse-line-to-point %)
      (line-seq r)))))

(defn -main
  [& args]
  (let [path (first args)
        points (read-points-from-file path)
        points (calculate-potentials points)]
    (doseq [center (find-centers points)] (println center))))
