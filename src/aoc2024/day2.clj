(ns aoc2024.day2
  (:gen-class))

(require '[clojure.string :as string]
         '[clojure.java.io :as io])

(def input (->> (string/split-lines (slurp (io/resource "day2.txt")))
                (map #(string/split % #"\s+"))
                (map #(map Integer/parseInt %))))

(def min-diff 1)
(def max-diff 3)

(defn get-cmp-fn [line]
  (if (> (first line) (second line))
    >
    <))

(defn is-safe
  ([line]
   (is-safe (rest line) (first line) (get-cmp-fn line)))
  ([line prev cmp-fn]
   (if (empty? line)
     true
     (let [curr (first line)
           diff (abs (- prev curr))
           in-range (and (>= diff min-diff) (<= diff max-diff))
           in-order (cmp-fn prev curr)]
       (if (and in-range in-order)
         (recur (rest line) curr cmp-fn)
         false)))))

(defn part1 []
  (->> input
       (filter is-safe)
       count))

;; Part 2

(defn is-safe-dampened
  ([line]
   (is-safe-dampened (rest line) (vector (first line)) (get-cmp-fn line) 0))
  ([line errors]
   (is-safe-dampened (rest line) (vector (first line)) (get-cmp-fn line) errors))
  ([remaining processed cmp-fn errors]
   (if (empty? remaining)
     true
     (let [curr (first remaining)
           prev (last processed)
           diff (abs (- prev curr))
           in-range (and (>= diff min-diff) (<= diff max-diff))
           in-order (cmp-fn prev curr)]
       (if (and in-range in-order)
         (recur (rest remaining) (conj processed curr) cmp-fn errors)
         (if (>= errors 1)
           false
           (is-safe-dampened (into processed (rest remaining)) (inc errors))))))))

(defn part2 []
  (->> input
       (filter #(or
                  (is-safe-dampened %)
                  (is-safe-dampened (reverse %))))
       count))
