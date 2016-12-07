(ns aoc2016.day07
  (:require [clojure.string :as s]
            [clojure.set :as set]))

(defn load-input [] (s/split (slurp "./data/day07.txt") #"\n"))

(defn is-palindrome? [string]
  (and (> (count (set string)) 1)
       (= (s/reverse string) string)))

(defn all-substrs [string len]
  (map s/join (partition len 1 string)))

(defn contains-abba? [string]
  (->> (all-substrs string 4)
       (map is-palindrome?)
       (some true?)
       (boolean)))

(defn inside-brackets [string]
  (let [bracs (re-seq #"\[.*?\]" string)]
    (vec (map #(subs % 1 (dec (count %))) bracs))))

(defn outside-brackets [string]
  (s/split string #"\[.*?\]"))

(defn supports-tls? [ip]
  (let [in (map contains-abba? (inside-brackets ip))
        out (map contains-abba? (outside-brackets ip))]
    (and (boolean (some true? out)) (not-any? true? in))))

(defn aba->bab [aba]
  (str (subs aba 1 2) (last aba) (subs aba 1 2)))

(defn supports-ssl? [ip]
  (let [in-aba (set (filter is-palindrome? (mapcat #(all-substrs % 3) (inside-brackets ip))))
        out-aba (set (filter is-palindrome? (mapcat #(all-substrs % 3) (outside-brackets ip))))]
    (-> (map aba->bab in-aba)
         (set)
         (set/intersection out-aba)
         (count)
         (> 0))))

(defn match-count [data func]
  (count (filter func data)))

(defn part-1 []
  (match-count (load-input) supports-tls?))

(defn part-2 []
  (match-count (load-input) supports-ssl?))
