(ns q06
  (:require [clojure.string :as str]))

(defn parse-tree
  "Parse the input file into a map of nodes and their children."
  [input-string]
  (reduce (fn [tree line]
            (let [[node-str children-str] (str/split line #":")
                  node (str/trim node-str) ;trimming whitespace
                  children (if children-str
                             (map str/trim (str/split children-str #","))
                             [])]
              (assoc tree node children)))
          {}
          (str/split-lines input-string)))

(defn paths-dfs
  [tree start-node visited]
  (cond
    (contains? visited start-node) []  ; Avoid cycles
    (= start-node "@") [["@"]]        ; Found an end
    :else
    (let [children (get tree start-node [])
          new-visited (conj visited start-node)]
      (->> children
           (mapcat (fn [child]
                     (let [child-paths (paths-dfs tree child new-visited)]
                       (when (seq child-paths)
                         (map #(cons start-node %) child-paths)))))
           (filter some?)
           vec))))

(defn find-unique-path
  "Find the path with the unique length"
  [tree]
  (->> (paths-dfs tree "RR" #{})
         (group-by count)
         (vals)
         (apply min-key count)
         first))

(defn part1
  [fname]
  (let [input-string (slurp fname)
        tree (parse-tree input-string)]
    (->> tree
         find-unique-path
         (str/join))))

(defn part2
  [fname]
  (let [input-string (slurp fname)
        tree (parse-tree input-string)]
    (->> tree
         find-unique-path
         (map first)
         (str/join))))

(comment
  (def testf1 "data/q06_p1_test.txt")
  (def inputf1 "data/q06_p1.txt")
  (def inputf2 "data/q06_p2.txt")
  (def inputf3 "data/q06_p3.txt")

  (part1 testf1)
  (part1 inputf1)
  (part2 inputf2)
  (part2 inputf3))
