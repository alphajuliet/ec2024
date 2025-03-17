(ns q05
  (:require [util :as util]
            [clojure.string :as str]))

(defn read-data 
  "Read in the data as a list of columns"
  [fname]
  (->> fname 
       slurp
       str/split-lines
       (mapv #(str/split % #" "))
       (mapv #(mapv Integer/parseInt %))
       util/T))

(defn insert-at
  "Insert a value at a specific index in a collection"
  [coll idx val]
  (vec (concat (take idx coll) [val] (drop idx coll))))

(defn delete-at
  "Delete the value at a specific index in a collection, and return both number and new collection"
  [vec idx]
  (let [val (nth vec idx)]
    {:val val 
     :vec (concat (take idx vec) (drop (inc idx) vec))}))

(defn- f [n x] 
  #_(- n (abs (- n x)))
    (abs (- (mod (+ x n) (* 2 n)) n)))

(defn front-numbers
  "Return the number formed from the front of each column"
  [columns]
  (->> columns
       (map first)
       str/join
       Integer/parseInt))

(defn move-number
  "Move the first number from src-col to the next column, in the appropriate position"
  [columns src-col]
  (let [dest-col (mod (inc src-col) (count columns))
        src (nth columns src-col)
        dest (nth columns dest-col)
        x (first src)
        new-dest (insert-at dest (f (count dest) (dec x)) x)]
    (-> columns
        (assoc src-col (vec (rest src)))
        (assoc dest-col new-dest))))

(defn play-rounds
  "Play n rounds"
  [columns n]
  (reduce (fn [cols col]
            (move-number cols (mod col (count cols))))
          columns
          (range n)))

(defn play-rounds-2
  "Play rounds until we see one number 2024 times"
  [columns]
  (loop [cols columns
         counts {}
         round 0]
    (let [cols' (move-number cols (mod round (count columns)))
          shout (front-numbers cols')
          counts' (update counts shout (fnil inc 0))]
      (if (= 2024 (get counts' shout))
        (list shout (inc round)) ; return a tuple
        ;; else
        (recur cols' counts' (inc round))))))

(defn part1
  [fname]
  (let [init (->> fname read-data)]
    (-> init
        (play-rounds 10)
        front-numbers)))

(defn part2
  [fname]
  (let [init (->> fname read-data)]
    (->> init
        (play-rounds-2)
        (apply *))))

(comment
  (def testf1 "data/q05_p1_test.txt")
  (def inputf1 "data/q05_p1.txt")
  (def testf2 "data/q05_p2_test.txt")
  (def inputf2 "data/q05_p2.txt")
  
  (part1 testf1)
  (part1 inputf1)
  (part2 testf2)
  (part2 inputf2))

;; The End