(ns aoc2024.day3
  (:gen-class))

(require '[clojure.string :as string]
         '[clojure.java.io :as io])

(def input (slurp (io/resource "day3.txt")))

(def mul #"mul\((\d{1,3}),(\d{1,3})\)")
(def instructions {:do   #"do\(\)"
                   :don't #"don't\(\)"})

;; Part1

(defn part1 []
  (->> (re-seq mul input)
       (map (fn [[_ a b]] (* (Integer/parseInt a) (Integer/parseInt b))))
       (reduce +)))

;; Part2

(defn instruction-indices [instruction]
  (let [m (re-matcher (get instructions instruction) input)]
    (loop [indices []]
      (if (.find m)
        (recur (conj indices [instruction (.start m)]))
        indices))))

(defn group-indices [indices]
  "Maps instructions to a list of range of the form (:do 0 :don't 100)"
  (map #(flatten %) (partition 2 1 indices)))

(defn is-do [index instruction-ranges]
  (let [instruction-range (first (filter #(<= (second %) index (last %)) instruction-ranges))
        instruction (first instruction-range)]
    (if-not (some? instruction) ; only happens when the mul comes before the last instruction, and is enabled
      true
      (= :do instruction))))

(defn sum-enabled-muls [instruction-ranges]
  (let [matcher (re-matcher mul input)]
    (loop [match (re-find matcher)
           result 0]
      (if-not match
        result
        (let [index (.start matcher)
              enabled (is-do index instruction-ranges)]
          (if enabled
            (recur (re-find matcher)
                   (+ result (* (Integer/parseInt (second match))
                                (Integer/parseInt (last match)))))
            (recur (re-find matcher) result)))))))

(defn part2 []
  (let [instructions (into (instruction-indices :do) (instruction-indices :don't))
        instruction-ranges (group-indices (sort-by second instructions))]
    (sum-enabled-muls instruction-ranges)))