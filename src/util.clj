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
  "Return all the row and column of the given value in a matrix."
  [m x]
  (let [[r c] (m/shape m)]
    (->> m
         flatten
         (map-indexed (fn [idx val]
                        (if (= val x) 
                          (vector (quot idx c) (mod idx c)) 
                          nil)))
         (remove nil?))))

;; The End
