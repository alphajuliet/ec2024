(ns util
  (:require [clojure.string :as str]))
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
  (reduce-kv (fn [acc k v]
              (assoc acc k (f v)))
            {}
            m))
  
;; The End
