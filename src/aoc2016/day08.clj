(ns aoc2016.day08
  (:require [incanter.core :as i]
            [clojure.string :as s]))

(def data
  (-> (slurp "./data/day08.txt")
      (s/split #"\n")))

(defn matrix [rows cols]
  (->> (repeat 0)
       (take rows)
       (vec)
       (repeat)
       (take cols)
       (i/to-dataset)))

(def realmatr (matrix 50 6))

(defn i->colname [i]
  (keyword (str "col-" i)))

(defn rotate [row amount]
  (if (zero? amount)
    (vec row)
    (rotate (flatten (conj [] (last row) (drop-last row)))
            (dec amount))))

(defn rotate-column [matr i amount]
  (let [col (rotate (i/$ i matr) amount)]
    (i/replace-column (i->colname i) col matr)))

(defn rotate-row [matr i amount]
  (let [newrow (rotate (i/$ i :all matr) amount)
        allrows (partition-by #(= % i) (range 0 (i/nrow matr)))
        firstrows (i/$ (first allrows) :all matr)
        lastrows (i/$ (last allrows) :all matr)]
    (if (= 0 i)
      (i/conj-rows newrow (i/$ (range 1 (i/nrow matr)) :all matr))
      (if (= i (dec (i/nrow matr)))
        (i/conj-rows (i/$ (range 0 i) :all matr) newrow)
        (i/conj-rows firstrows newrow lastrows)))))

(defn rect [matr cols rows]
  (loop [m matr
         c cols]
    (if (zero? c)
      m
      (let [cname (i->colname (dec c))
            oldc (i/$ cname m)
            newc (concat (take rows (repeat 1))
                         (drop rows (i/$ cname m)))]
        (recur (i/replace-column cname newc m)
               (dec c))))))

(defn solve [matr instr]
  (loop [m matr
         ins instr]
    (if (empty? ins)
      m
      (let [in (s/split (first ins) #"\s+")]
        (if (= "rect" (first in))
          (let [c (Integer. (first (s/split (last in) #"x")))
                r (Integer. (last (s/split (last in) #"x")))]
                (recur (rect m c r)
                       (rest ins)))
          (let [nos (re-seq #"\d+" (first ins))
                coord (Integer. (first nos))
                amount (Integer. (last nos))]
            (if (.contains (first ins) "column")
              (recur (rotate-column m coord amount)
                     (rest ins))
              (recur (rotate-row m coord amount)
                     (rest ins)))))))))

(defn part-1 []
  (->> (solve realmatr data)
       (vec)
       (flatten)
       (filter map?)
       (map vals)
       (flatten)
       (reduce +)))

(defn part-2 []
  (i/view (solve realmatr data)))
