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

(defn- f [n x] (- n (abs (- n x))))

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
        new-dest (insert-at dest (f (count src)(dec x)) x)]
    (-> columns
        (assoc src-col (vec (rest src)))
        (assoc dest-col new-dest))))

(defn play-rounds
  "Play a single round"
  [columns rounds]
  (reduce (fn [cols col]
            (move-number cols (mod col (count cols))))
          columns
          (range rounds)))

(defn part1
  [fname]
  (let [init (->> fname read-data)]
    (-> init
        (play-rounds 10)
        front-numbers)))

(comment
  (def testf "data/q05_p1_test.txt")
  (def inputf "data/q05_p1.txt")
  
  (part1 testf)
  (part1 inputf))

;; The End