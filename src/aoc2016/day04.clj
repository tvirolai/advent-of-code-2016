(ns aoc2016.day04
  (:require [clojure.string :as s]))

(defn input []
  (-> (slurp "./data/day04.txt")
      (s/split #"\n")))

(defn get-sector-id [entry]
  (Integer. (re-find #"\d+" entry)))

(defn get-checksum [entry]
  (re-find #"\w+" (re-find #"\[.*\]" entry)))

(defn split-entry [entry]
  (s/split entry #"-"))

(defn count-checksum [entry]
  (->> (split-entry entry)
       (drop-last)
       (map #(s/split % #""))
       (flatten)
       (frequencies)
       (into (sorted-map))
       (sort-by val)
       (partition-by last)
       (reverse)
       (map keys)
       (flatten)
       (take 5)
       (s/join "")))
  
(defn part-1 []
  (->> (input)
       (filter #(= (count-checksum %) (get-checksum %)))
       (map get-sector-id)
       (reduce +)))

(defn rotate-char [amount char]
  (let [alphabet (into [] "abcdefghijklmnopqrstuvwxyz")
        newindex (+ amount (.indexOf alphabet char))]
    (last (take (inc newindex) (cycle alphabet)))))

(defn decrypt-string [amount string]
  (s/join "" (map (partial rotate-char amount) string)))

(defn decrypt-entry [entry]
  (let [id (get-sector-id entry)
        words (drop-last (split-entry entry))]
    (s/join " " (map (partial decrypt-string id) words))))

(defn part-2 []
  (->> (input)
       (filter #(.contains (decrypt-entry %) "north"))
       (first)
       (get-sector-id)))
