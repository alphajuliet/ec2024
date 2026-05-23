(ns q09
  (:require [clojure.edn :as edn]
            [clojure.string :as str]))

(defn read-data
  [f]
  (->> f
       slurp
       str/split-lines
       (map edn/read-string)))

(defn greedy-change
  "Greedy change-making. Returns a map of coin->count using the largest
   coins first. Assumes `coins` is a canonical system for correctness;
   on a non-canonical system it returns *a* solution, not necessarily
   minimal. Throws if the target cannot be made exactly."
  [coins target]
  {:pre [(every? pos? coins) (not (neg? target))]}
  (loop [[c & more :as cs] (sort > coins)
         remaining target
         acc {}]
    (cond
      (zero? remaining) acc
      (nil? c) (throw (ex-info "Cannot make exact change"
                               {:coins coins :target target
                                :remaining remaining}))
      :else (let [n (quot remaining c)]
              (recur more
                     (- remaining (* n c))
                     (if (pos? n) (assoc acc c n) acc))))))

(defn optimal-change
  "Minimum-coin change-making via DP. Returns {:count n, :coins {c->k, ...}}
   for the optimal solution, or nil if `target` cannot be made exactly.
   Works for any coin system (canonical or not). O(target * |coins|) time."
  [coins target]
  {:pre [(every? pos? coins) (not (neg? target))]}
  (let [coins (vec (distinct coins))
        ;; dp[v] = min coins to make v, or nil if unreachable
        ;; choice[v] = which coin was used to reach v optimally
        init-dp     (assoc (vec (repeat (inc target) nil)) 0 0)
        init-choice (vec (repeat (inc target) nil))
        [dp choice]
        (reduce
         (fn [[dp choice] v]
           (let [candidates (for [c coins
                                  :when (and (<= c v) (some? (dp (- v c))))]
                              [c (inc (dp (- v c)))])]
             (if (seq candidates)
               (let [[best-c best-n] (apply min-key second candidates)]
                 [(assoc dp v best-n) (assoc choice v best-c)])
               [dp choice])))
         [init-dp init-choice]
         (range 1 (inc target)))]
    (when-let [n (dp target)]
      {:count n
       :coins (loop [v target, acc {}]
                (if (zero? v)
                  acc
                  (let [c (choice v)]
                    (recur (- v c) (update acc c (fnil inc 0))))))})))

(defn part1
  "Solution for part 1"
  [fname]
  (let [targets (read-data fname)
        stamps [1 3 5 10]]
    (->> targets
         (map #(greedy-change stamps %))
         (map vals)
         flatten
         (apply +))))
  
(defn part2
  "Solution for part 2"
  [fname]
  (let [targets (read-data fname)
        stamps [1 3 5 10 15 16 20 24 25 30]]
     (->> targets
         (map #(optimal-change stamps %))
         (map :count)
         (apply +)))) 

(comment
  (def testf1 "data/q09_p1_test.txt")
  (def inputf1 "data/q09_p1.txt")

  (def testf2 "data/q09_p2_test.txt")
  (def inputf2 "data/q09_p2.txt")

  (part1 testf1)
  (part1 inputf1)

  (part2 testf2)
  (part2 inputf2))
  
;; The End
