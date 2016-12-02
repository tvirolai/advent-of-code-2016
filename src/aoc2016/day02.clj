(ns aoc2016.day02
  (:require [clojure.string :as str]))

(def input
  (map #(re-seq #"[RLUD]" %)
       (str/split (slurp "./data/day02.txt") #"\n")))

(def grid1
  [{:x 0 :y 0 :val 5}
   {:x 1 :y 0 :val 6}
   {:x -1 :y 0 :val 4}
   {:x -1 :y 1 :val 1}
   {:x 0 :y 1 :val 2}
   {:x 1 :y 1 :val 3}
   {:x -1 :y -1 :val 7}
   {:x 0 :y -1 :val 8}
   {:x 1 :y -1 :val 9}])

(def grid2
  [{:x 0 :y 0 :val 7}
   {:x 1 :y 0 :val 8}
   {:x 2 :y 0 :val 9}
   {:x -1 :y 0 :val 6}
   {:x -2 :y 0 :val 5}
   {:x -1 :y 1 :val 2}
   {:x 0 :y 1 :val 3}
   {:x 1 :y 1 :val 4}
   {:x 0 :y 2 :val 1}
   {:x -1 :y -1 :val "A"}
   {:x 0 :y -1 :val "B"}
   {:x 1 :y -1 :val "C"}
   {:x 0 :y -2 :val "D"}])

(defn get-val [coord grid]
  "Return a value by coordinates."
  (let [entry (filter #(= (dissoc % :val) coord) grid)]
    (:val (first entry))))

(defn next-coord [coord instr]
  "Return a new coordinate map from previous coordinates and an instruction."
  (let [x (:x coord)
        y (:y coord)]
    (case instr
      "R" {:x (inc x) :y y}
      "L" {:x (dec x) :y y}
      "U" {:x x :y (inc y)}
      "D" {:x x :y (dec y)})))

(defn valid-coord? [coord grid]
  (let [x (:x coord)
        y (:y coord)]
    (not (empty? (filter #(and (= x (:x %)) (= y (:y %))) grid)))))

(defn process-instr [firstcoord instr grid]
  "Takes a coordinate and a sequence of instructions, returns a number and a coordinate as a map"
  (loop [coord firstcoord
         dirs instr]
    (if (empty? dirs)
      {:val (get-val coord grid) :coord coord}
      (let [next (next-coord coord (first dirs))]
        (if (valid-coord? next grid)
          (recur next (rest dirs))
          (recur coord (rest dirs)))))))

(defn solve [coord instr grid]
  (loop [coord {:x 0 :y 0}
        dirs input
        nos []]
    (if (empty? dirs)
      (str/join "" nos)
      (let [res (process-instr coord (first dirs) grid)]
        (recur (:coord res)
               (rest dirs)
               (conj nos (:val res)))))))

(defn part-1 []
  (solve {:x 0 :y 0} input grid1))

(defn part-2 []
  (solve {:x -2 :y 0} input grid2))
