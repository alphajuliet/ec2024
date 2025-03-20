(ns q07 
  (:require [clojure.string :as str]
            [util :as util]))

(defn read-data
  [data]
  (reduce (fn [acc line]
           (let [[name path] (str/split line #":")]
             (assoc acc name (map first (str/split path #",")))))
         {}
         (str/split-lines data)))

(defn eval-track
  "Evaluate the score for a track over n segments"
  [track n]
  (loop [t (cycle track)
         acc []
         ctr 10
         i 0]
    (if (>= i n) 
      (reduce + acc)
      ;; else
      (let [delta (case (first t)
                    \+ 1
                    \- -1
                    \= 0)
            ctr' (+ ctr delta) ]
        (recur (rest t) (conj acc ctr') ctr' (inc i))))))

(defn part1
  [fname]
  (let [plans (read-data (slurp fname))]
    (->> plans
         (util/map-vals #(eval-track % 10))
         (sort-by val)
         (map first)
         reverse
         str/join)))

(comment
  (def testf1 "data/q07_p1_test.txt")
  (def inputf1 "data/q07_p1.txt")
  (part1 testf1)
  (part1 inputf1))
;; The End