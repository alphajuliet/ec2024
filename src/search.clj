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

;; The End