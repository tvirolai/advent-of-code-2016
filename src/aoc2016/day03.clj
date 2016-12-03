(ns aoc2016.day03
  (:require [clojure.math.combinatorics :as combo]
            [clojure.string :as str]))

(defn- parse-stringlist [list]
  (map #(Integer/parseInt %) list))

(def input
  (map (comp parse-stringlist #(re-seq #"\d+" %))
       (-> (slurp "./data/day03.txt") (str/split #"\n"))))

(defn is-triangle? [entry]
  (->> (combo/permutations entry)
       (map #(< (first %) (reduce + (rest %))))
       (every? true?)))

(defn solve [data]
  (->> (map is-triangle? data)
       (filter true?)
       (count)))

(defn part-1 []
  (solve input))

(defn part-2 []
  (let [data (partition 3 (flatten (apply mapv vector input)))]
    (solve data)))
