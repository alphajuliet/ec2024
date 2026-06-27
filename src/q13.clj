(ns q13
  (:require [clojure.string :as str]
            [clojure.core.matrix :as m]
            [search :as sch]
            [util :as util]))

(defn read-data
  "Read in the data file and return a matrix of strings."
  ;; read-data : String -> Vector (Vector String)
  [f]
  (->> f
       slurp
       str/split-lines
       (map #(str/split % #""))))

(defn children
  "Find all the children of the given node, i.e. digits or 'E'. 
   Only go up, right, or down."
  ;; children : Matrix -> [Int Int] -> [[Int Int]]
  [m rc]
  (let [nn [[-1 0] [0 1] [1 0] [0 -1]]
        cc (for [n nn] (mapv + rc n))
        [rmax cmax] (m/shape m)]
    (->> cc
         (filter (fn [[r c]]
                   (and (<= 0 r (dec rmax))
                        (<= 0 c (dec cmax))
                        (re-matches #"\d|E" (m/mget m r c))))))))

(defn cost
  "Find the cost of moving from the given node to the given child."
  ;; cost : Vector (Vector a) -> Coord -> Coord -> Int
  [m [r0 c0] [r1 c1]]
  (let [curr (m/mget m r0 c0)
        next (m/mget m r1 c1)
        e0   (if (= curr "S") 0 (Integer/parseInt curr))
        e1   (if (= next "E") 0 (Integer/parseInt next))
        d    (abs (- e1 e0))]
    (inc (min d (- 10 d)))))

(defn shortest-path
  "Find the length of the shortest path"
  ;; shortest-path : Vector (Vector a) -> [Int Int] -> [Int Int] -> Int
  [m start end]
  (-> (sch/shortest-path (partial children m) (partial cost m) 1000 start end)
      second
      (get end)))

(defn part1
  "Solution for part 1"
  [fname]
  (let [m (read-data fname)
        start (first (util/mfind-all m "S"))
        end (first (util/mfind-all m "E"))]
    (println "start:" start "end:" end)
    (shortest-path m start end)))

(defn part2
  "Solution for part 2"
  [fname]
  fname)

(comment
  (def testf1 "data/q13_p1_test.txt")
  (def inputf1 "data/q13_p1.txt")

  (part1 testf1)
  (part1 inputf1)

  (def testf2 "data/q13_p2_test.txt")
  (def inputf2 "data/q13_p2.txt")

  (part2 testf2)
  (part2 inputf2))

;; The End
