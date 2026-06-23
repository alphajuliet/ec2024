(ns q12
  (:require [clojure.string :as str]
            [clojure.core.matrix :as m]
            [util :as util]))

(defn read-data
  [f]
  (->> f
       slurp
       str/split-lines
       (map #(str/split % #""))))

(defn find-targets
  "Find the xy coordinates of the given target string in the field"
  [str m]
  (let [[rr _] (m/shape m)]
    (map (fn [[r c]] [(dec c) (dec (- rr r))]) 
         (util/mfind-all m str))))

(defn intersects-y
  "Do the lines intersect? If yes, return the strength, else return nil."
  [[_ ys] [xt yt]]
  (let [a (+ xt yt)
        b (* 2 ys)
        y (/ (+ a b) 3)]
    (if (int? y)
      (/ (- a y) 2)
      nil)))

(defn ranking
  [segments targets]
  (->> (for [s segments
             t targets]
         (when-let [st (intersects-y s t)]
           (* (second s) st)))
       (remove nil?)
       (apply +)))

(defn part1
  "Solution for part 1"
  [fname]
  (let [field (read-data fname)
        segments [[0 1] [0 2] [0 3]]
        targets (find-targets "T" field)]
    (ranking segments targets)))
     
(defn part2
  "Solution for part 2"
  [fname]
  (let [field (read-data fname)
        segments [[0 1] [0 2] [0 3]]
        targets (find-targets "T" field)
        rocks (find-targets "H" field)
        a (ranking segments targets)
        b (* 2 (ranking segments rocks))]
    (+ a b)))

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
