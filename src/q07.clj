(ns q07
  (:require [clojure.string :as str]
            [util :as util]))

(defn read-plans
  "Parse the plans data into a map of name -> sequence of actions"
  [data]
  (reduce (fn [acc line]
            (let [[name path] (str/split line #":")]
              (assoc acc name (map first (str/split path #",")))))
          {}
          (str/split-lines data)))

(defn read-track
  "Parse the track data into a sequence representing the track perimeter"
  [track]
  (let [lines (str/split-lines track)
        height (count lines)
        width (count (first lines))]
    (vec
     (concat
      ;; Top row, left to right
      (seq (first lines))
      ;; Right side, top to bottom (excluding first and last char)
      (map #(nth % (dec width)) (take (dec height) (rest lines)))
      ;; Bottom row, right to left (excluding first char)
      (rest (reverse (seq (last lines))))
      ;; Left side, bottom to top (excluding first and last char)
      (map first (reverse (rest (butlast lines))))))))

(defn eval-plans
  "Evaluate the score for a plan over n segments"
  [track n]
  (loop [t (cycle track)
         acc []
         ctr 10
         i 0]
    (if (>= i n)
      (reduce + acc)
      (let [delta (case (first t)
                    \+ 1
                    \- -1
                    (\= \S) 0)
            ctr' (max 0 (+ ctr delta))]
        (recur (rest t) (conj acc ctr') ctr' (inc i))))))

(defn eval-track
  "Evaluate a plan over a single loop of track"
  [plan track init-power]
  (let [track-len (count track)]
    (loop [p (cycle plan)
           t (rest (cycle track))
           acc []
           pwr init-power
           i 0]
      (if (>= i track-len)
        {:total (reduce + acc) :power pwr}
        (let [delta (case (first p)
                      \+ 1
                      \- -1
                      \= 0)
              delta' (case (first t)
                       \+ 1
                       \- -1
                       (\= \S) delta)
              pwr' (max 0 (+ pwr delta'))]
          (recur (rest p) (rest t) (conj acc pwr') pwr' (inc i)))))))

(defn eval-loops
  "Score n loops"
  [plan track loops]
  (reduce (fn [acc _]
            (let [{:keys [:total :power]} (eval-track plan track (:power acc))]
              (-> acc
                  (update :total + total)
                  (assoc :power power))))
          {:total 0 :power 10} ; Starting conditions
          (range loops)))

(defn part1
  "Solution for part 1"
  [fname]
  (let [plans (read-plans (slurp fname))]
    (->> plans
         (util/map-vals #(eval-plans % 10))
         (sort-by val >)
         (map first)
         str/join)))

(defn part2
  "Solution for part 2"
  [plan-fname track-fname]
  (let [plans (read-plans (slurp plan-fname))
        track (read-track (slurp track-fname))]
    (->> plans
         (util/map-vals #(eval-loops % track 10))
         (util/map-vals :total)
         (sort-by val >)
         (map first)
         str/join)))

(comment
  (def testf1 "data/q07_p1_test.txt")
  (def inputf1 "data/q07_p1.txt")

  (part1 testf1)
  (part1 inputf1)

  (def testf2 "data/q07_p1_test.txt")
  (def testf2_track "data/q07_p2_test_track.txt")
  (def inputf2 "data/q07_p2.txt")
  (def inputf2_track "data/q07_p2_track.txt")

  (part2 testf2 testf2_track)
  (part2 inputf2 inputf2_track))
;; The End
