(ns q04
  (:require [util :as util]
            [clojure.math :refer [floor-div]]))

(defn get-strikes
  [heights]
  (let [z (apply min heights)]
    (->> heights
         (map #(- % z))
         (reduce +))))

(defn get-strikes3
  [heights]
  (let [h (sort heights)
        mid (floor-div (count h) 2)
        median (nth h mid)]
    (->> heights
         (map #(abs (- % median)))
         (reduce +))))

(defn part1
  [fname]
  (->> fname
       util/read-data
       (map Integer/parseInt)
       get-strikes))

(def part2 part1)

(defn part3
  [fname]
  (->> fname
       util/read-data
       (map Integer/parseInt)
       get-strikes3))

(comment
  (part1 "data/q04_p1_test.txt")
  (part1 "data/q04_p1.txt")

  (part2 "data/q04_p2.txt")

  (part3 "data/q04_p3_test.txt")
  (part3 "data/q04_p3.txt")
  )