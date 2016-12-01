(ns aoc2016.day01
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(defn parse-instr [entry]
  {:dir (subs entry 0 1) :amount (Integer. (subs entry 1))})

(def input (map parse-instr (str/split (str/trim (slurp "./data/day01.txt")) #", ")))

(defn count-distance [state]
  (+ (Math/abs (:x state)) (Math/abs (:y state))))

(defn next-coord [state instr]
  "Takes a state map (direction and coordinates) and an instruction map, returns a new state."
  (let [turn (str/join [(:dir state) (:dir instr)])
        x (:x state)
        y (:y state)
        amount (:amount instr)]
    (case turn
      ("NR" "SL") {:dir "E" :x (+ x amount) :y y}
      ("ER" "WL") {:dir "S" :x x :y (- y amount)}
      ("SR" "NL") {:dir "W" :x (- x amount) :y y}
      ("WR" "EL") {:dir "N" :x x :y (+ y amount)})))

(defn part-1 []
  (loop [state {:dir "N" :x 0 :y 0}
         moves input]
    (if (empty? moves)
      (count-distance state)
      (recur
       (next-coord state (first moves))
       (rest moves)))))

(defn inter-states [state1 state2]
  "This monstrosity returns a sequence of intermediate states between two coords."
  (let [x1 (:x state1)
        y1 (:y state1)
        x2 (:x state2)
        y2 (:y state2)]
    (if (= x1 x2)
      (map #(assoc {:x x1} :y %) (flatten (conj (range (inc y2) y1) (range (dec y2) y1 -1))))
      (map #(assoc {:y y1} :x %) (flatten (conj (range (inc x2) x1) (range (dec x2) x1 -1)))))))

(defn already-visited? [states state]
  (filter #(and (= (:x state) (:x %))
                (= (:y state) (:y %))) states))

(defn part-2 []
  (loop [state {:dir "N" :x 0 :y 0}
         moves input
         visited []]
    (let [next (next-coord state (first moves))
          inters (inter-states state next)
          same (set/intersection (set visited) (set inters))]
      (if-not (empty? same)
        (count-distance (first same))
        (recur (next-coord state (first moves))
               (rest moves)
               (conj (concat visited inters) state))))))
