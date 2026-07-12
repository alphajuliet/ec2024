(ns q15
  (:require [clojure.core.matrix :as m]
            [clojure.string :as str]
            [search :as sch]
            [util :as util]))

(defn read-data
  [f]
  (->> f
       slurp
       str/split-lines
       (map #(str/split % #""))))

(defn find-entry
  "Find the entry point in the top wall"
  [wall]
  (->> wall
       (map-indexed #(vector %1 %2))
       (filter #(= (second %) "."))
       first
       first))

(defn children
  "Find all valid neighbours of the given node (.) in all four directions."
  ;; children : Matrix -> [Int Int] -> [[Int Int]]
  [m rc]
  (let [nn [[-1 0] [0 1] [1 0] [0 -1]]
        cc (for [n nn] (mapv + rc n))
        [rmax cmax] (m/shape m)]
    (->> cc
         (filter (fn [[r c]]
                   (and (<= 0 r (dec rmax))
                        (<= 0 c (dec cmax))
                        (re-matches #"[.A-Z]" (m/mget m r c))))))))

(defn max-states
  "An upper bound on the number of reachable states in the grid, used to
    let searches run to completion regardless of grid size."
  [m]
  (apply * (m/shape m)))

(defn shortest-path
  "Find the shortest path from the start to the target"
  [m start target]
  (-> (sch/shortest-path
       (partial children m)
       (constantly 1)
       (max-states m)
       start
       target)
      second
      (get target)))

(defn herb-locations
  "Find all the available herb locations by type."
  [m]
  (->> m
       m/to-vector
       (filter #(re-matches #"[A-Z]" %))
       set
       (map #(util/mfind-all m %))))

(defn shortest-round-trip
  "Find the shortest path from the start, visiting all given nodes
    in any order."
  [m start targets]
  (sch/shortest-round-trip-visiting-all
   (partial children m)
   (constantly 1)
   (max-states m)
   start
   targets))

(defn shortest-round-trip-one-per-class
  "Find the shortest round trip from the start, visiting exactly one
    node from each class (e.g. one herb of each type) in any order."
  [m start classes]
  (sch/shortest-round-trip-visiting-one-per-class
   (partial children m)
   (constantly 1)
   (max-states m)
   start
   classes))

(defn part1
  "Solution for part 1"
  [fname]
  (let [grid (read-data fname)
        entry (vector 0 (find-entry (first grid)))
        herb-locs (util/mfind-all grid "H")]
    (->> herb-locs
         (map #(shortest-path grid entry %))
         (apply min)
         (* 2))))

(defn part2
  "Solution for part 2"
  [fname]
  (let [grid (read-data fname)
        entry (vector 0 (find-entry (first grid)))
        herb-locs (herb-locations grid)]
    (:cost (shortest-round-trip-one-per-class grid entry herb-locs))))

(comment
  (def testf1 "data/q15_p1_test.txt")
  (def inputf1 "data/q15_p1.txt")

  (part1 testf1)
  (part1 inputf1)

  (def testf2 "data/q15_p2_test.txt")
  (def inputf2 "data/q15_p2.txt")

  (part2 testf2)
  (part2 inputf2)

  (def inputf3 "data/q15_p3.txt")
  (part2 inputf3))
;; The End