(ns q16
  (:require [clojure.string :as str]))

(defn read-column
  "Read a column of three characters at a given column number."
  ;; read-column : [String] -> Int -> [String]
  [strs n]
  (let [start (* n 4)]
     (map #(subs % start (+ start 3)) strs)))

(defn read-data
  "Read this file input data into a list of steps and the symbols for each wheel."
  ;; read-data : String -> {:steps [Int], :syms [[String]]}
  [f]
  (let [[a b] (->> f
                   slurp
                   str/split-lines
                   (split-with (comp not empty?)))
        posns (-> a
                  first
                  (str/split #",")
                  (#(mapv Integer/parseInt %)))
        syms (->> posns
                  (map-indexed (fn [idx _] (read-column (rest b) idx)))
                  (map (partial remove str/blank?)))]
                        
    {:steps posns :syms syms}))

(defn score-syms
  "Score the current set of symbols"
  [syms idxs]
  (->> idxs
       (mapv nth syms)
       (str/join "")
       frequencies
       vals
       (filter #(>= % 3))
       (map #(- % 2))
       (apply +)))

(defn pull
  "Pull the lever once and update the state using the configuration."
  [{:keys [steps syms]} {:keys [posns] :as state}]
  (let [lengths (mapv count syms) 
        posns' (as-> posns <>
                    (mapv + <> steps)
                    (mapv mod <> lengths))]
    (-> state
        (assoc :posns posns')
        (update :coins + (score-syms syms posns')))))

(defn part1
  "Solution for part 1"
  [fname]
  (let [cfg (->> fname read-data)
        init-state {:posns (vec (repeat (count (:steps cfg)) 0))
                    :coins 0}]
    (->> (reduce
          (fn [st _] (pull cfg st))
          init-state
          (range 100))
         (:posns)
         (map nth (:syms cfg))
         (str/join " "))))

(defn part2
  "Solution for part 2"
  [_])

(comment
  (def testf1 "data/q16_p1_test.txt")
  (def inputf1 "data/q16_p1.txt")

  (part1 testf1)
  (part1 inputf1) ; ">,< ^,< >,< *:<"

  (def x (read-data inputf1))
  (def init {:posns [0 0 0 0] :coins 0})
  (pull x init)

  (def testf2 "data/q16_p2_test.txt")
  (def inputf2 "data/q16_p2.txt")

  (part2 testf2)
  (part2 inputf2))

;; The End
