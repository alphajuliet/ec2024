(ns q03
  (:require [util :as util]))

(defn get-value
  "Get the value of the grid at the given location or '.' if out of bounds"
  [grid [r c]]
  (let [[r-max c-max] (util/dims grid)]
    (if (or (neg-int? r) (neg-int? c)
            (>= r r-max) (>= c c-max))
      \.
      (-> grid
          (nth r)
          (nth c)))))

(defn surrounded-hv?
  "Returns true if the given grid location is surrounded by x or x-1"
  [grid [r c] x]
  (let [dirs [[1 0] [-1 0] [0 1] [0 -1]]
        nn (map (fn [[dr dc]] [(+ r dr) (+ c dc)]) dirs)
        nn' (filter (fn [[r c]] (and (>= r 0) (< r (count grid)) 
                                     (>= c 0) (< c (count (first grid))))) 
                    nn)]
    (and (<= (dec (int x)) (int (get-value grid [r c])) (int x))
         (every? (fn [[r c]] (= x (get-value grid [r c]))) nn'))))

(defn surrounded-diag?
  "Returns true if the given grid location is surrounded by x or x-1"
  [grid [r c] x]
  (let [dirs [[1 0] [-1 0] [0 1] [0 -1] [1 1] [1 -1] [-1 1] [-1 -1]]
        nn (map (fn [[dr dc]] [(+ r dr) (+ c dc)]) dirs)
        nn' (filter (fn [[r c]] (and (>= r 0) (< r (count grid)) 
                                     (>= c 0) (< c (count (first grid))))) 
                    nn)]
    (and (<= (dec (int x)) (int (get-value grid [r c])) (int x))
         (every? (fn [[r c]] (= x (get-value grid [r c]))) nn'))))

(defn update-block
  "Update a single block"
  [grid [r c] surr-fn]
  (let [ch (get-value grid [r c])]
    (case ch
      \. ch
      \# \1
      (if (surr-fn grid [r c] ch)
        (char (inc (int ch)))
        ch))))

(defn scan-grid
  "A single-pass update of all the blocks in the grid"
  [grid surr-fn]
  (let [[rmax cmax] (util/dims grid)]
    (->> (for [r (range rmax)]
           (for [c (range cmax)]
             (update-block grid [r c] surr-fn)))
         (mapv #(apply str %)))))

(defn update-grid
  "Update the grid until it stops changing"
  [grid surr-fn]
  (loop [g grid]
    (let [g' (scan-grid g surr-fn)]
      (if (= g g')
        (apply concat g')
        (recur (scan-grid g' surr-fn))))))

(defn count-blocks
  "Count the blocks at all depths"
  [grid]
  (let [g (->> grid 
               (mapv #(- (int %) 48))
               (filterv pos-int?)
               frequencies)]
    (->> (map * (keys g) (vals g))
         (reduce +))))

(defn part1
  [fname]
  (-> fname
      util/read-data
      (update-grid surrounded-hv?)
      count-blocks))

(def part2 part1)
       
(defn part3
  [fname]
  (-> fname
      util/read-data
      (update-grid surrounded-diag?)
      count-blocks))

(comment
  (part1 "data/q03_p1_test.txt")
  (part1 "data/q03_p1.txt")

  (part2 "data/q03_p2.txt")

  (part3 "data/q03_p1_test.txt")
  (part3 "data/q03_p1_test2.txt")
  (part3 "data/q03_p3.txt"))
  ;; 11337 has wrong length and wrong first digit.
  
;; The End