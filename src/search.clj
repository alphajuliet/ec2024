(ns search
  (:require [clojure.set :as set]
            [clojure.data.priority-map :as pm])
  (:gen-class))
;; From https://github.com/NicMcPhee/a-star-search/blob/master/src/search/algorithms.clj

(defn breadth-first-search
  "Breadth-first search algorithm implementation."
  [children-fn max-states start-state goal-state]
  (loop [max-states max-states
         frontier (conj clojure.lang.PersistentQueue/EMPTY start-state)
         visited #{}
         came-from {}]
    (if (or (neg? max-states)
            (empty? frontier)
            (= (peek frontier) goal-state))
      came-from
      (let [current (peek frontier)
            children (set (children-fn current))
            unvisited-children (set/difference children visited)
            new-frontier (reduce conj (pop frontier) unvisited-children)
            new-visited (set/union children visited)
            new-came-from (reduce #(assoc %1 %2 current) came-from unvisited-children)]
        (recur (- max-states (count unvisited-children))
               new-frontier
               new-visited
               new-came-from)))))

(defn shortest-path
  "Find the shortest path from start-state to goal-state using Dijkstra's algorithm."
  [children-fn cost-fn max-states start-state goal-state]
  (loop [max-states max-states
         frontier (pm/priority-map start-state 0)
         came-from {}
         cost-so-far {start-state 0}]
    (if (or (neg? max-states)
            (empty? frontier)
            (= (first (peek frontier)) goal-state))
      [came-from cost-so-far]
      (let [current (first (peek frontier))
            current-cost (cost-so-far current)
            children (set (children-fn current))
            children-costs (reduce #(assoc %1 %2 (+ current-cost (cost-fn current %2))) {} children)
            children-to-add (filter #(or (not (contains? cost-so-far %))
                                         (< (children-costs %) (cost-so-far %))) children)
            new-cost-so-far (reduce #(assoc %1 %2 (children-costs %2)) cost-so-far children-to-add)
            new-frontier (reduce #(assoc %1 %2 (children-costs %2)) (pop frontier) children-to-add)
            new-came-from (reduce #(assoc %1 %2 current) came-from children-to-add)]
        (recur (- max-states (count children-to-add))
               new-frontier
               new-came-from
               new-cost-so-far)))))

(defn heuristic-search 
  "Heuristic search algorithm implementation (A* search)."
  [children-fn heuristic-fn start-state goal-state & {:keys [max-states] :or {max-states 1000000}}]
  (loop [frontier (pm/priority-map start-state 0)
         came-from {}
         visited #{}]
    (if (or (empty? frontier)
            (>= (count came-from) max-states)
            (= (first (peek frontier)) goal-state))
      came-from
      (let [current (first (peek frontier))
            children (set (children-fn current))
            unvisited-children (clojure.set/difference children visited)
            ;; heuristics (map (partial heuristic-fn goal-state) unvisited-children)
            new-frontier (reduce #(assoc %1 %2 (heuristic-fn goal-state %2)) (pop frontier) unvisited-children)
            new-came-from (reduce #(assoc %1 %2 current) came-from unvisited-children)
            new-visited (clojure.set/union children visited)]
        (recur new-frontier new-came-from new-visited)))))

(defn extract-path 
  "Extract the path from the came-from map."
  [came-from start-state goal-state]
  (loop [current-state goal-state
         path []]
    (cond
      (nil? current-state) nil
      (= current-state start-state) (reverse (conj path start-state))
      :else (recur (get came-from current-state)
                   (conj path current-state)))))

(defn find-all-paths
  "Find all paths from start-state to goal-state using a modified Dijkstra's algorithm."
  [children-fn cost-fn max-states start-state goal-state]
  (loop [max-states max-states
         frontier (pm/priority-map start-state 0)
         came-from {start-state #{}}
         cost-so-far {start-state 0}]
    (if (or (neg? max-states)
            (empty? frontier)
            (and (contains? cost-so-far goal-state)
                 (> (second (peek frontier)) (cost-so-far goal-state))))
      [came-from cost-so-far]
      (let [current (first (peek frontier))
            current-cost (cost-so-far current)
            children (set (children-fn current))
            children-costs (reduce #(assoc %1 %2 (+ current-cost (cost-fn current %2))) {} children)
            children-to-process (for [child children]
                                  [child (children-costs child)])
            new-state (reduce (fn [state [child cost]]
                                (let [{:keys [came-from cost-so-far frontier]} state]
                                  (cond
                                    (or (not (contains? cost-so-far child))
                                        (< cost (cost-so-far child)))
                                    {:came-from   (assoc came-from child #{current})
                                     :cost-so-far (assoc cost-so-far child cost)
                                     :frontier    (assoc frontier child cost)}
                                    (= cost (cost-so-far child))
                                    (assoc state :came-from (update came-from child conj current))

                                    :else state)))
                              {:came-from   came-from
                               :cost-so-far cost-so-far
                               :frontier    (pop frontier)}
                              children-to-process)]
        (recur (dec max-states)
               (:frontier new-state)
               (:came-from new-state)
               (:cost-so-far new-state))))))

(defn extract-all-paths
  "Extract all paths from the came-from map."
  [came-from start-state goal-state]
  (letfn [(dfs [current path]
            (if (= current start-state)
              (list (reverse path))
              (when-let [predecessors (seq (get came-from current))]
                (mapcat #(dfs % (conj path current)) predecessors))))]
    (dfs goal-state [])))

(defn- pairwise-shortest-costs
  "Cost matrix {[a b] cost} of shortest-path costs between every ordered
   pair of distinct, mutually-reachable nodes in `nodes`."
  [children-fn cost-fn max-states nodes]
  (into {}
        (for [a nodes
              :let [[_ cost-so-far] (shortest-path children-fn cost-fn max-states a nil)]
              b nodes
              :when (and (not= a b) (contains? cost-so-far b))]
          [[a b] (cost-so-far b)])))

(defn shortest-round-trip-visiting-all
  "Shortest round trip that starts at `start-state`, visits every node in
   `must-visit` (in any order), and returns to `start-state`. `must-visit`
   should NOT include `start-state` itself -- it is the fixed departure and
   return point, not one of the stops to route between.
   Returns {:order [start ... start] :cost n}, or nil if some node is
   unreachable from another.
   Reduces the dense graph to a pairwise shortest-path distance matrix,
   then solves the resulting travelling-salesman cycle with Held-Karp
   dynamic programming: O(k^2 * 2^k) for k = (count must-visit), so this
   is only practical for k up to roughly 15-20."
  [children-fn cost-fn max-states start-state must-visit]
  (let [nodes (vec (distinct (remove #{start-state} must-visit)))
        n     (count nodes)]
    (if (zero? n)
      {:order [start-state start-state] :cost 0}
      (let [dist      (pairwise-shortest-costs children-fn cost-fn max-states
                                                (conj nodes start-state))
            cost      (fn [a b] (get dist [a b] Double/POSITIVE_INFINITY))
            full-mask (dec (bit-shift-left 1 n))
            dp        (atom {})
            parent    (atom {})]
        ;; base case: mask with only node j set, reached directly from start
        (doseq [j (range n)]
          (swap! dp assoc [(bit-shift-left 1 j) j] (cost start-state (nodes j))))
        ;; build up dp[mask][j] = min cost from start-state visiting exactly
        ;; the nodes in mask, ending at (nodes j), in increasing mask popcount
        (doseq [mask (range 1 (inc full-mask))
                :when (> (Long/bitCount mask) 1)
                j (range n)
                :when (bit-test mask j)]
          (let [prev-mask (bit-clear mask j)
                best (reduce
                      (fn [best i]
                        (if (and (bit-test prev-mask i)
                                 (contains? @dp [prev-mask i]))
                          (let [c (+ (@dp [prev-mask i]) (cost (nodes i) (nodes j)))]
                            (if (< c (:cost best))
                              {:cost c :from i}
                              best))
                          best))
                      {:cost Double/POSITIVE_INFINITY :from nil}
                      (range n))]
            (when (:from best)
              (swap! dp assoc [mask j] (:cost best))
              (swap! parent assoc [mask j] (:from best)))))
        ;; close the loop: pick the best node to end on before returning to start
        (let [end (apply min-key
                          #(+ (get @dp [full-mask %] Double/POSITIVE_INFINITY)
                              (cost (nodes %) start-state))
                          (range n))
              min-cost (some-> (@dp [full-mask end]) (+ (cost (nodes end) start-state)))]
          (when (and min-cost (< min-cost Double/POSITIVE_INFINITY))
            (loop [mask full-mask j end order '()]
              (if (nil? j)
                {:order (vec (concat [start-state] (map nodes order) [start-state]))
                 :cost min-cost}
                (recur (bit-clear mask j) (@parent [mask j]) (cons j order))))))))))

(defn- flatten-classes
  "Flattens `classes` (a seq of seqs of candidate nodes) into a pair of
   parallel vectors: every candidate node, and the index of the class it
   belongs to."
  [classes]
  (reduce
   (fn [acc [class-idx class-nodes]]
     (reduce (fn [acc node]
               (-> acc
                   (update :nodes conj node)
                   (update :class-of conj class-idx)))
             acc
             class-nodes))
   {:nodes [] :class-of []}
   (map-indexed vector classes)))

(defn shortest-round-trip-visiting-one-per-class
  "Shortest round trip that starts at `start-state`, visits exactly one
   node from each class in `classes` (a seq of seqs of candidate nodes --
   which node is chosen per class is left to the search), and returns to
   `start-state`. `classes` should NOT include `start-state`.
   Returns {:order [start chosen... start] :cost n}, or nil if no
   selection of representatives is fully reachable.
   Generalizes Held-Karp to a bitmask over *classes* rather than over
   individual nodes, so the combinatorial cost depends only on the number
   of classes k (2^k), not on how many candidate nodes populate each
   class: O(2^k * N^2) for k classes and N total candidate nodes -- this
   is exact, not an approximation, and stays practical up to roughly
   k <= 20 regardless of how large each class is."
  [children-fn cost-fn max-states start-state classes]
  (let [k (count classes)]
    (if (zero? k)
      {:order [start-state start-state] :cost 0}
      (let [{:keys [nodes class-of]} (flatten-classes classes)
            nodes     (vec nodes)
            class-of  (vec class-of)
            n         (count nodes)
            dist      (pairwise-shortest-costs children-fn cost-fn max-states
                                                (conj nodes start-state))
            cost      (fn [a b] (get dist [a b] Double/POSITIVE_INFINITY))
            full-mask (dec (bit-shift-left 1 k))
            dp        (atom {})
            parent    (atom {})]
        ;; base case: visit node j alone, mask = just its own class bit
        (doseq [j (range n)]
          (swap! dp assoc [(bit-shift-left 1 (class-of j)) j] (cost start-state (nodes j))))
        ;; relax forward: from (mask, u) extend to (mask | bit(class v), v)
        ;; for any v whose class isn't already represented in mask -- mask
        ;; only ever grows, so ascending numeric order is a valid processing
        ;; order (every source state is finalised before it's used)
        (doseq [mask (range 1 (inc full-mask))
                u (range n)
                :when (contains? @dp [mask u])
                v (range n)
                :when (not (bit-test mask (class-of v)))]
          (let [new-mask (bit-or mask (bit-shift-left 1 (class-of v)))
                c (+ (@dp [mask u]) (cost (nodes u) (nodes v)))]
            (when (< c (get @dp [new-mask v] Double/POSITIVE_INFINITY))
              (swap! dp assoc [new-mask v] c)
              (swap! parent assoc [new-mask v] u))))
        ;; close the loop: pick the best node to end on before returning to start
        (let [reached (filter #(contains? @dp [full-mask %]) (range n))]
          (when (seq reached)
            (let [end (apply min-key #(+ (@dp [full-mask %]) (cost (nodes %) start-state))
                              reached)
                  min-cost (+ (@dp [full-mask end]) (cost (nodes end) start-state))]
              (loop [mask full-mask j end order '()]
                (if (nil? j)
                  {:order (vec (concat [start-state] (map nodes order) [start-state]))
                   :cost min-cost}
                  (recur (bit-clear mask (class-of j)) (@parent [mask j]) (cons j order)))))))))))

;; The End