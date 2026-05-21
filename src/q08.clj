(ns q08
  (:require [clojure.string :as str]
            [util]))

(defn read-data
  "Read the data from the input file."
  [f]
  (-> f
      slurp
      str/trim-newline
      Integer/parseInt))

(defn thickness
  "Recursive function for thickness of each layer."
  [a n m]
  (-> a (* n) (mod m)))

(defn layer-blocks-seq
  "A lazy sequence of the number of blocks in each layer, given the note `n` and modulus `m`."
  [n m]
  (let [a (iterate #(thickness % n m) 1)
        b (iterate #(+ 2 %) 1)]
    (map * a b)))

(defn part1
  "Solution for part 1"
  [fname]
  (let [a (read-data fname)
        i (-> a Math/sqrt Math/ceil int)
        a' (* i i)
        w (dec (* 2 i))]
    (* w (- a' a))))

(defn part2
  "Solution for part 2"
  [fname acolytes max-blocks]
  (let [note (read-data fname)
        blocks (layer-blocks-seq note acolytes)
        totals-seq (reductions + blocks)
        blocks-used (util/take-until #(> % max-blocks) totals-seq)
        width (dec (* 2 (count blocks-used)))
        Δblocks (- (last blocks-used) max-blocks)]
    (* width Δblocks)))

(comment
  (def testf1 "data/q08_p1_test.txt")
  (def inputf1 "data/q08_p1.txt")

  (def testf2 "data/q08_p2_test.txt")
  (def inputf2 "data/q08_p2.txt")

  (part1 testf1)
  (part1 inputf1)

  (part2 testf2 5 50)
  (part2 inputf2 1111 20240000))
  
;; The End
