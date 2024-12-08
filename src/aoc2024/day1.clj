(ns aoc2024.day1
  (:gen-class))

(require '[clojure.string :as string]
         '[clojure.java.io :as io])

(def input (->> (string/split-lines (slurp (io/resource "day1.txt")))
                (map #(string/split % #"\s+"))))

(defn build-id-colls [id-pair colls]
  (vector (conj (first id-pair) (Integer/parseInt (first colls)))
          (conj (second id-pair) (Integer/parseInt (second colls)))))

(defn calculate-distances
  ([left-ids right-ids]
   (calculate-distances left-ids right-ids 0))
  ([left-ids right-ids result]
   (let [left (first left-ids)
         right (first right-ids)]
     (if (empty? left-ids)
       result
       (recur (rest left-ids)
              (rest right-ids)
              (+ result (abs (- left right))))))))

(defn calc-similarity-score [left-id right-ids]
  (* left-id (count (filter (fn [right-id] (= left-id right-id)) right-ids))))

(defn part1 []
  (let [id-colls (reduce build-id-colls [[] []] input)
        left-ids (sort (first id-colls))
        right-ids (sort (second id-colls))]
    (calculate-distances left-ids right-ids)))

(defn part2 []
  (let [id-colls (reduce build-id-colls [[] []] input)
        left-ids (sort (first id-colls))
        right-ids (sort (second id-colls))]
    (->> left-ids
         (map #(calc-similarity-score % right-ids))
         (reduce +))))

