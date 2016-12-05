(ns aoc2016.day05
  (:require [digest :refer :all]
            [clojure.string :as s]))

(def input "cxdnnyjw")

(def data 
  '("00000fe1e92080b9951b053e70e31fcb" "000007c827126c81fa664211693f2540" "000007880153f1b804481a39a6d2e86a" "00000a6e225253b6aaa8f20efbaad8b5" "00000096164643e2e0fbf5a91bfd7f06" "00000e77a8b223b2d149990fb634bd74" "000006ec13bc03b597beee4fa9352176" "00000ee477915a03c15f93ac53769648"))

(def candidates
  (->> (range)
       (map (comp digest/md5 #(str input %)))
       (filter #(= "00000" (subs % 0 5)))))

(defn part-1 []
  (->> candidates
       (take 8)
       (map #(subs % 5 6))
       (s/join)))

(defn get-index [string]
  "Returns the index value, if valid, and 10 otherwise"
  (let [index (try (Integer. (subs string 5 6))
             (catch NumberFormatException e))]
    (if (number? index) index 10)))

(defn parse-result [resmap]
  (s/join (vals (into (sorted-map) resmap))))

(defn part-2 []
  (loop [cands candidates
         indices (zipmap (range 8) (range 8))
         results {}]
    (let [cand (first cands)
          i (get-index cand)
          value (subs cand 6 7)]
      (if (empty? indices)
        (parse-result results)
        (if (contains? indices i)
          (do
            (prn (str "Now is the time to be jolly - and patient: " (inc (count results)) " character(s) found..."))
            (recur (rest cands)
                   (dissoc indices i)
                   (assoc results i value)))
          (recur (rest cands)
                 indices
                 results))))))
