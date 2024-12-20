(ns aoc2024.day7
  (:require [clojure.string :as string]
            [clojure.java.io :as io]))

(def input (->> (string/split-lines (slurp (io/resource "day7.txt")))
                (map #(string/split % #":\s*"))
                (map (fn [[l r]] [(Long/parseLong l) (map #(Integer/parseInt %) (string/split r #"\s+"))]))))

(defn solvable-helper [target current remaining]
  (if (empty? remaining)
    (= target current)
    (let [next (first remaining)
          rest-vals (rest remaining)]
      (or (solvable-helper target (+ current next) rest-vals)
          (solvable-helper target (* current next) rest-vals)))))

(defn is-solvable [[target [first-val & values]]]
  (solvable-helper target first-val values))

(defn part1 []
  (reduce + (map #(first %) (filter is-solvable input))))

;; Part2

(defn concatenate [a b]
  (Long/parseLong (str a b)))

(defn solvable-helper-concat [target current remaining]
  (if (empty? remaining)
    (= target current)
    (let [next (first remaining)
          rest-vals (rest remaining)]
      (or (solvable-helper-concat target (+ current next) rest-vals)
          (solvable-helper-concat target (* current next) rest-vals)
          (solvable-helper-concat target (concatenate current next) rest-vals)))))

(defn is-solvable-with-concat [[target [first-val & values]]]
  (solvable-helper-concat target first-val values))

(defn part2 []
  (reduce + (map #(first %) (filter is-solvable-with-concat input))))