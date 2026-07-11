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
                        (re-matches #"[.H]" (m/mget m r c))))))))

(defn shortest-path
  "Find the shortest path from the entry to the target"
  [m start target]
  (-> (sch/shortest-path (partial children m)
                         (constantly 1)
                         1000
                         start
                         target)
      second
      (get target)))
  
(defn part1
  "Solution for part 1"
  [fname]
  (let [grid (read-data fname)
        entry (vector 0 (find-entry (first grid)))
        herbs (util/mfind-all grid "H")]
    (->> herbs
         (map #(shortest-path grid entry %))
         (apply min)
         (* 2))))

(defn part2
  "Solution for part 2"
  [fname])
  
(comment
  (def testf1 "data/q15_p1_test.txt")
  (def inputf1 "data/q15_p1.txt")

  (def testf2 "data/q15_p2_test.txt")
  (def inputf2 "data/q15_p2.txt")

  (part1 testf1)
  (part1 inputf1)

  (part2 testf2)
  (part2 inputf2))

;; The End