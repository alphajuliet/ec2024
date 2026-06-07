(ns q10
  (:require [clojure.string :as str]
            [util :as util]))

(defn read-data-p1
  "Read input data"
  [f]
  (let [chars (->> f
                   slurp
                   (str/split-lines)
                   (mapv #(str/split % #"")))]
    chars))

(defn read-data-p2
  "Read the input data for part 2 into a list of runes"
  [f]
  (->> f
       slurp
       str/split-lines
       (remove #(zero? (count %)))
       (map #(str/split % #" "))
       util/T
       (mapv #(partition 8 %))
       (apply concat) 
       (util/mapmap #(str/split % #""))))

(defn coords
  "Map each character in a grid to a list of [row col] coordinates where it appears"
  [chars]
  (reduce
    (fn [acc [r c ch]]
      (update acc ch (fnil conj []) [r c]))
    {}
    (for [r (range (count chars))
          c (range (count (nth chars r)))]
      [r c (get-in chars [r c])])))

(defn ipp
  "Find the points of intersection of orthogonal lines through the two given points."
  [[[a b] [c d]]]
  [[a d] [c b]])

(defn middle-only
  "Return the middle coordinate, i.e. where the '.' are located"
  [[[a b] [c d]]]
  (if (and (<= 2 a 5) (<= 2 b 5))
    [a b]
    [c d]))

(defn effective-power
  "Return the effective power of a given string"
  [w]
  (->> (str/split w #"")
       (map (comp #(- % 64) int first))
       (map * (range 1 (inc (count w))))
       (apply +)))

(defn extract-word [chars]
  (->> chars
       coords
       (#(dissoc % "*"))
       (#(dissoc % "."))
       (util/map-vals (comp middle-only ipp))
       ; (util/map-vals middle-only)
       (sort-by #(second (val %)))
       (sort-by #(first (val %)))
       keys
       (str/join)))

(defn part1
  "Solution for part 1"
  [fname]
  (let [chars (read-data-p1 fname)]
    (extract-word chars)))

(defn part2
  "Solution for part 2"
  [fname]
  (let [runes (->> fname
                   read-data-p2)
        xf (comp (map extract-word) (map effective-power))]
    (transduce xf + runes)))

(def testf1 "data/q10_p1_test.txt")
(def inputf1 "data/q10_p1.txt")
(def testf2 "data/q10_p2_test.txt")
(def inputf2 "data/q10_p2.txt")

(comment
  (part1 testf1) 
  (part1 inputf1)

  (part2 testf2) 
  (part2 inputf2))

;; The End
