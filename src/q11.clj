(ns q11
  (:require [clojure.string :as str]
            [util :as util]))
            
(defn create-rule
  "Convert a substition rule <src>: <dest> into a hash"
  ;; String -> Map Char (Map Char Int)
  [s]
  (let [[k v] (str/split s #":")
        dest (-> v
                 (str/replace #"," "")
                 frequencies
                 (assoc (first k) -1))]
    {(first k) dest}))

(defn create-rule-p3
  "Convert a substition rule <src>: <dest> into a hash"
  ;; String -> Map String (Map String Int)
  [s]
  (let [[k v] (str/split s #":")
        dest (-> v
                 (str/split #",")
                 frequencies
                 (assoc k -1))]
    {k dest}))

(defn read-data
  "Read the substitution rules into a hash map."
  ;; String -> Map Char (Map Char Int)
  [f]
  (->> f
       slurp
       str/split-lines
       (into {} (map create-rule))))

(defn read-data-p3
  "Read the part 3 substitution rules into a hash map."
  ;; String -> Map Char (Map String Int)
  [f]
  (->> f
       slurp
       str/split-lines
       (into {} (map create-rule-p3))))

(defn convert-one
  "Convert the given character in the input according to its rule."
  ;; Map Char Int -> Map Char Int -> Char
  [rules input ch]
  (let [rule (get rules ch)]
    (util/map-vals #(* % (get input ch)) rule)
    #_(if (pos-int? (get input ch))
        (merge-with + input rule)
        ;; else
        input)))

(defn convert
  "Convert one generation into the next via map+reduce. 
   Don't let counts go negative."
  ;; Map Char Int -> Map Char Int -> Map Char Int  
  [rules input]
  {:pre [(and (map? input) (map? rules))]}
  (let [chars (keys input)]
    (->> chars
         (map #(convert-one rules input %))
         (reduce (partial merge-with +) input))))
         
(defn part1
  "Solution for part 1"
  [fname]
  (let [rules (read-data fname)]
    (->> {\A 1}
         (iterate #(convert rules %))
         (take 5)
         last
         vals
         (apply +))))
         
(defn part2
  "Solution for part 2"
  [fname]
  (let [rules (read-data fname)]
    (->> {\Z 1}
         (iterate #(convert rules %))
         (take 11)
         last
         vals
         (apply +))))

(defn total
  "Calculate the total count of all characters after 20 generations."
  [rules init]
  (->> (reduce
        (fn [acc _] (convert rules acc))
        init
        (range 20))
       vals
       (apply +)))

(defn part3
  "Solution for part 3"
  [fname]
  (let [rules (read-data-p3 fname)
        totals (map #(total rules {% 1}) (keys rules))]
    (- (apply max totals) (apply min totals))))
             
(comment
  (def testf1 "data/q11_p1_test.txt")
  (def inputf1 "data/q11_p1.txt")
  (part1 testf1)
  (part1 inputf1)

  (def testf2 "data/q11_p2_test.txt")
  (def inputf2 "data/q11_p2.txt")
  (part2 inputf2)
  
  (def testf3 "data/q11_p3_test.txt")
  (def inputf3 "data/q11_p3.txt")
  (part3 testf3)
  (part3 inputf3))
  ;; not 947608161037

;; The End