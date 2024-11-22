(ns util
  (:require [clojure.string :as str]))
  
(defn mapmap
  "Map a function over a collection of collections"
  [f coll]
  (mapv (partial mapv f) coll))

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

(def T (partial apply mapv vector))
  
;; The End