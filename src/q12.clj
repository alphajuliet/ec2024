(ns q12
  (:require [clojure.string :as str]
            [clojure.core.matrix :as m]
            [util :as util]))

(def interleave-2d 
  (comp (partial partition 2) interleave))

(defn read-data
  [f]
  (->> f
       slurp
       str/split-lines
       (map #(str/split % #""))))

(defn find-targets
  "Find the xy coordinates of the targets in the field"
  [m]
  (let [[rr _] (m/shape m)]
    (map (fn [[r c]] [(dec c) (dec (- rr r))]) 
         (util/mfind-all m "T"))))

#_(defn trajectory
    "Return a list of the coordinates of the trajectory given a given start point, base row, and strength."
    [[r0 c0] base-row strength]
    (let [rise (interleave-2d (range (dec r0) (- r0 (inc strength)) -1)
                              (range (inc c0) (+ c0 (inc strength)) 1))
          [r1 c1] (last rise)
          plateau (interleave-2d (repeat strength r1) (range (inc c1) (+ c1 (inc strength)) 1))
          [r2 c2] (last plateau)
          fall (interleave-2d (range (inc r2) (+ (- base-row r2)) 1)
                              (range (inc c2) (+ c2 (- base-row r2)) 1))]
      (concat rise plateau fall)))

(defn intersects-y
  "Do the lines intersect? If yes, return the strength, else return nil."
  [[_ ys] [xt yt]]
  (let [a (+ xt yt)
        b (* 2 ys)
        y (/ (+ a b) 3)]
    (if (int? y)
      (/ (- a y) 2)
      nil)))

(defn part1
  "Solution for part 1"
  [fname]
  (let [field (read-data fname)
        segments [[0 1] [0 2] [0 3]]
        targets (find-targets field)]
    (->> (for [s segments
               t targets]
           (when-let [st (intersects-y s t)] 
             (* (second s) st))) 
         (remove nil?)   
         (apply +))))
     

(defn part2
  "Solution for part 2"
  [fname])
  
(comment
  (def testf1 "data/q12_p1_test.txt")
  (def inputf1 "data/q12_p1.txt")
  (def testf2 "data/q12_p2_test.txt")
  (def inputf2 "data/q12_p2.txt")

  (part1 testf1)
  (part1 inputf1)

  (part2 testf2)
  (part2 inputf2))
;; The End
