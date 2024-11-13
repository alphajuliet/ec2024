;; q02.clj
(ns q02
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(defn- get-words
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
  (let [words (get-words text)
        source (->> text
                    (drop 2) 
                    first)]
    (count-matches source words)))

(defn part2
  [text]
  (let [words (get-words text)
        source (drop 2 text)]
    (->> source
         (map #(find-words % words))
         (map #(apply set/union %))
         (map count)
         (reduce +))))
                  
(comment
  (->> "data/q02_p1.txt"
       slurp
       str/split-lines
       part1)
  (->> "data/q02_p2.txt"
       slurp
       str/split-lines
       part2))
  
;; The End