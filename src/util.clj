(ns util
  (:require [clojure.core.matrix :as m]
            [clojure.string :as str]))

(defn mapmap
  "Map a function over a collection of collections"
  [f coll]
  (mapv #(mapv f %) coll))

(defn read-data
  "Reads the data file and returns a list of strings"
  [fname]
  (->> fname
       slurp
       str/split-lines))

(defn dims
  "Return the dimensions of the grid"
  [grid]
  [(count grid) (count (first grid))])

(def T
  "Transpose a 2D collection"
  (partial apply mapv vector))

(defn manhattan
  "Calculate the Manhattan distance between two points"
  [[x1 y1] [x2 y2]]
  (+ (Math/abs (- x1 x2))
     (Math/abs (- y1 y2))))

(defn mst-length
  "Compute the total edge length of the minimum spanning tree connecting
  coords, using Prim's algorithm. distance-fn defaults to Manhattan
  distance."
  ([coords] (mst-length coords manhattan))
  ([coords distance-fn]
   (let [start (first coords)]
     (loop [dists (into {} (map (fn [p] [p (distance-fn start p)]) (rest coords)))
            total 0]
       (if (empty? dists)
         total
         (let [[nearest d] (apply min-key val dists)]
           (recur (into {}
                        (map (fn [[p pd]] [p (min pd (distance-fn nearest p))]))
                        (dissoc dists nearest))
                  (+ total d))))))))

(defn map-vals
  "Map a function over the values of a map"
  [f m]
  (reduce-kv
    (fn [acc k v]
      (assoc acc k (f v)))
    {}
    m))

(defn take-until
  "Returns a lazy sequence of successive items from coll until
  (pred item) returns true, including that item. pred must be
  free of side-effects."
  [pred coll]
  (lazy-seq
    (when-let [s (seq coll)]
      (if (pred (first s))
        (cons (first s) nil)
        (cons (first s) (take-until pred (rest s)))))))

(defn mfind-all
  "Return all coordinates of the given value x in the matrix m."
  [m x]
  (let [[_ c] (m/shape m)]
    (->> m
         flatten
         (map-indexed (fn [idx val]
                        (if (= val x) 
                          (vector (quot idx c) (mod idx c)) 
                          nil)))
         (remove nil?))))

;; The End
