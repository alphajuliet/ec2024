(ns q14
  (:require [clojure.string :as str]
            [util :as util]))

(defn read-data
  "Read the input data and transform into something useful."
  ;; read-data : String -> Vector String
  [f]
  (-> f
      slurp
      str/trim-newline
      (str/split #",")))

(defn read-data2
   "Read in a series of trees"
   [f]
   (->> f
        slurp
        (str/split-lines)
        (map #(str/split % #","))))

(defn move
  "Update the current position with the given move"
  ;; move : [Int Int Int] -> String -> [Int Int Int]
  [[x y z] m]
  (let [dir (first m)
        n (Integer/parseInt (subs m 1))]
    (case dir
      \U [x (+ y n) z]
      \D [x (- y n) z]
      \L [(- x n) y z]
      \R [(+ x n) y z]
      \F [x y (+ z n)]
      \B [x y (- z n)])))

(defn path-points
  "Return all the points on the path between two points, which can be positive or negative.
   Assume that the path is orthogonal to one axis, i.e. only one coordinate is changing."
  ;; all-points : [Int Int Int] -> [Int Int Int] -> [[Int Int Int]]
  [[x0 y0 z0] [x1 y1 z1]]
  (let [dx (- x1 x0)
        dy (- y1 y0)
        dz (- z1 z0)
        steps (max (abs dx) (abs dy) (abs dz))]
    (for [i (range (inc steps))]
      [(+ x0 (* i (Long/signum dx)))
       (+ y0 (* i (Long/signum dy)))
       (+ z0 (* i (Long/signum dz)))])))

(defn all-path-points
  "Return all points visited by following a path described by the list of vertices."
  ;; [[Int Int Int]] -> [[Int Int Int]]
  [vertices]
  (->> vertices
       (partition 2 1)
       (map #(apply path-points %))))

(defn part1
  "Solution for part 1"
  [fname]
  (let [moves (read-data fname)]
    (->> moves
         (map second)
         (apply max))))

(defn part2
  "Solution for part 2"
  [fname]
  (let [trees (read-data2 fname)
        all-moves (map #(reductions move [0 0 0] %) trees)]
    (->> all-moves
         (map all-path-points)
         (apply concat)
         (apply concat)
         set
         count
         dec)))

(comment
  (def testf1 "data/q14_p1_test.txt")
  (def inputf1 "data/q14_p1.txt")

  (def testf2 "data/q14_p2_test.txt")
  (def inputf2 "data/q14_p2.txt")

  (part1 testf1)
  (part1 inputf1)

  (part2 testf2)
  (part2 inputf2))

;; The End