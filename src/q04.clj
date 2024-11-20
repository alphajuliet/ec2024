(ns q04
  (:require [util :as util]))

(defn get-strikes
  [heights]
  (let [z (apply min heights)]
    (->> heights
         (map #(- % z))
         (reduce +))))

(defn part1
  [fname]
  (->> fname
       util/read-data
       (map Integer/parseInt)
       get-strikes))

(def part2 part1)

(comment
  (part1 "data/q04_p1_test.txt")
  (part1 "data/q04_p1.txt")
  (part2 "data/q04_p2.txt")
  )