(ns aoc2016.day06
  (:require [clojure.string :as s]))

(defn load-input []
  (s/split (slurp "./data/day06.txt") #"\n"))

(defn freq-by-index [data]
  (->> data
       (map #(s/split % #""))
       (mapcat #(map-indexed (fn [i x] [i x]) %))
       (frequencies)
       (sort-by val)
       (reverse)))

(defn solve [data]
  (->> data 
       (take 8)
       (sort-by first)
       (flatten)
       (filter string?)
       (s/join)))

(defn part-1 []
  (solve (freq-by-index (load-input))))

(defn part-2 []
  (solve (reverse (freq-by-index (load-input)))))
