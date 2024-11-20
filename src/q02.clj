;; q02.clj
(ns q02
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(defn- get-keywords
  "Extract the keywords from the text"
  [text]
  (-> text
      first
      (str/split #":")
      last
      (str/split #",")))

(defn- count-matches 
  "Count the matches for each word in the source text"
  [source words]
  (->> words
       (map #(-> % 
                 re-pattern 
                 (re-seq source) 
                 count))
       (reduce +)))

(defn get-symbols 
  "Get the sequence of n-character substrings"
  [n text]
  (let [max-i (- (count text) (- n 1))]
    (map #(vector (range % (+ % n)) (subs text % (+ % n))) (range max-i))))

(defn find-words
  [source words]
  (->> (for [w words
             :let [strs (get-symbols (count w) source)]]
         (concat (filter #(= w (second %)) strs)
                 (filter #(= w (str/reverse (second %))) strs)))
       (apply concat)
       (map (comp set first))))

(defn part1
  [text]
  (let [words (get-keywords text)
        source (->> text
                    (drop 2) 
                    first)]
    (count-matches source words)))

(defn part2
  [text]
  (let [words (get-keywords text)
        source (drop 2 text)]
    (->> source
         (map #(find-words % words))
         (map #(apply set/union %))
         (map count)
         (reduce +))))
        
(defn- dims
  "Get the dimensions of a grid"
  [grid]
  (let [rows (count grid)
        cols (count (first grid))]
    [rows cols]))

(defn get-horizontal
  "Extract a substring given a start position and length, in either direction (± len), 
  and wrapping around as necessary"
  [grid [r c] n]
  (let [s (nth grid r)
        len (count s)]
    (if (pos? n)
      (->> s cycle (drop c) (take n) (apply str))
      (->> s str/reverse cycle (drop (dec (- len c))) (take (- n)) (apply str)))))

(defn get-vertical
  "Given a grid of characters, return the vertical characters from the given 
   position and length. Do not wrap around top to bottom."
  [grid [r c] n]
  (let [[max-r _] (dims grid)
        indices (if (pos? n)
                  (range n)
                  (range 0 n -1))]
    (if (or (neg? (+ r n))
            (> (+ r n) max-r))
      ""
      (->> indices
           (map #(mod (+ r %) max-r))
           (map #(nth (nth grid %) c))
           (apply str)))))

(defn get-strings
  "Given a grid of characters, return the strings in each direction of length n
   from the given position. Wrap around if needed."
  [grid [r c] n]
  (let [fwd (get-horizontal grid [r c] n)
        bwd (get-horizontal grid [r c] (- n))
        down (get-vertical grid [r c] n)
        up (str/reverse (get-vertical grid [r c] (- n)))]
    [fwd bwd down up]))

(defn find-candidates
  "Find all candidates of keywords in all directions in the grid."
  [keywords grid]
  (let [[max-r max-c] (dims grid)]
    (for [w keywords
          r (range max-r)
          c (range max-c)
          :when (= (first w) (-> grid (nth r) (nth c)))]
      [[r c] (get-strings grid [r c] (count w))])))

(defn get-keyword-coords
  "Filter out the keywords and generate their coordinates."
  [keywords candidates]
  (for [w keywords
        c candidates
        :let [n (count w)
              [row col] (first c)
              [fwd bwd down up] (second c)]]
      (vector
       (if (= w fwd)  (mapv #(vector row %) (range col (+ col n))) [])
       (if (= w bwd)  (mapv #(vector row %) (range col (- col n) -1)) [])
       (if (= w down) (mapv #(vector % col) (range row (+ row n))) [])
       (if (= w up)   (mapv #(vector % col) (range row (- row n) -1)) []))))

(defn part3
  [text]
  (let [keywords (get-keywords text)
        grid (drop 2 text)]
    (->> (find-candidates keywords grid)
         (get-keyword-coords keywords)
         (apply concat)
         (remove empty?)
         (apply concat)
         set
         count)))
                  
(comment
  (->> "data/q02_p1.txt"
       slurp
       str/split-lines
       part1)
  (->> "data/q02_p2.txt"
       slurp
       str/split-lines
       part2)
  (->> "data/q02_p3_test.txt"
       slurp
       str/split-lines
       part3)
  (time (->> "data/q02_p3.txt"
             slurp
             str/split-lines
             part3)))
  
;; Part 3 is not 11047
;; The End