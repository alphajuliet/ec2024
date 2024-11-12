#!/usr/bin/env bb
(ns q01)

(defn lookup
  [animal]
  (case animal
    \A 0
    \B 1
    \C 3
    \D 5))

(defn adjust-for-x
  "Count the x's and adjust the result accordingly"
  [coll]
  (let [len (count coll)
        x-count (count (filter #{\x} coll))
        coll' (map lookup (remove #{\x} coll))]
    (->> coll'
         (map #(+ (- len x-count 1) %))
         (reduce +))))

(defn part1
  [s]
  (->> s
       (partition 1)
       flatten
       (map lookup)
       (reduce +)))

(defn potions
  "Calculate the number of potions required"
  [p s]
  (->> s
       (partition p)
       (map adjust-for-x)
       (map (partial max 0))
       (reduce +)))

(->> "data/q01_p1.txt" slurp part1)
(->> "data/q01_p2.txt" slurp (potions 2))
(->> "data/q01_p3.txt" slurp (potions 3))

# The End