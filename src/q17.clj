(ns q17
  (:require [clojure.string :as str]
            [util :as util]))

(defn read-data
  "Return the coordinates of all the stars in the grid."
  [f]
  (->> f
       slurp
       str/split-lines
       (map #(str/split % #""))
       (#(util/mfind-all % "*"))))

(defn part1
  "Solution for part 1"
  [fname]
  (let [coords (read-data fname)]
    (->> coords
         util/mst-length
         (+ (count coords)))))

(comment
  (def testf1 "data/q17_p1_test.txt")
  (def inputf1 "data/q17_p1.txt")

  (def inputf2 "data/q17_p2.txt")
 
  (part1 testf1)
  (part1 inputf1)

  (part1 inputf2))

;; The End