(ns q17
  (:require [clojure.string :as str]
            [util :as util]))

;; (def indices (comp range count))

(defn read-data
  "Return the coordinates of all the stars in the grid."
  [f]
  (->> f
       slurp
       str/split-lines
       (map #(str/split % #""))
       (#(util/mfind-all % "*"))))

(defn find-closest-pairs
  "For each point, find the distance to the nearest neighbour."
  [coords]
  (for [a coords]
    (->> coords
         (map (partial util/manhattan a))
         (remove zero?)
         (apply min))))

(defn part1
  "Solution for part 1"
  [fname]
  (let [coords (read-data fname)]
    (->> coords
         util/mst-length
         (+ (count coords)))))

(defn part2
  "Solution for part 2"
  [fname])

(comment
  (def testf1 "data/q17_p1_test.txt")
  (def inputf1 "data/q17_p1.txt")

  (def testf2 "data/q17_p2_test.txt")
  (def inputf2 "data/q17_p2.txt")
 
  (part1 testf1)
  (part1 inputf1)

  (part2 testf2)
  (part2 inputf2))
;; The End
