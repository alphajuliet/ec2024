(ns q09
  (:require [clojure.edn :as edn]
            [clojure.string :as str]))

(defn read-data
  [f]
  (->> f
       slurp
       str/split-lines
       (map edn/read-string)))

(defn greedy-change
  "Greedy change-making. Returns a map of coin->count using the largest
   coins first. Assumes `coins` is a canonical system for correctness;
   on a non-canonical system it returns *a* solution, not necessarily
   minimal. Throws if the target cannot be made exactly."
  [coins target]
  {:pre [(every? pos? coins) (not (neg? target))]}
  (loop [[c & more :as cs] (sort > coins)
         remaining target
         acc {}]
    (cond
      (zero? remaining) acc
      (nil? c) (throw (ex-info "Cannot make exact change"
                               {:coins coins :target target
                                :remaining remaining}))
      :else (let [n (quot remaining c)]
              (recur more
                     (- remaining (* n c))
                     (if (pos? n) (assoc acc c n) acc))))))

(defn part1
  "Solution for part 1"
  [fname]
  (let [targets (read-data fname)]
    (->> targets
         (map #(greedy-change [1 3 5 10] %))
         (map vals)
         flatten
         (apply +))))
  
(defn part2
  "Solution for part 2"
  [fname])
  
(comment
  (def testf1 "data/q09_p1_test.txt")
  (def inputf1 "data/q09_p1.txt")

  (def testf2 "data/q09_p1_test.txt")
  (def inputf2 "data/q09_p2.txt")

  (part1 testf1)
  (part1 inputf1)

  (part2 testf2)
  (part2 inputf2))
;; The End
