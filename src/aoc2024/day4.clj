(ns aoc2024.day4
  (:gen-class))

(require '[clojure.string :as string]
         '[clojure.java.io :as io])

(def rows (string/split-lines (slurp (io/resource "day4.txt"))))
(def cols (map #(apply str %) (apply map vector rows)))
(def xmas #"XMAS")
(def x-mas #"MAS")

(defn get-char [x y]
  (nth
    (nth rows y nil)
    x
    nil))

(def directions {:down-right [+ +]
                 :down-left  [- +]
                 :up-right   [+ -]
                 :up-left    [- -]})

(defn get-diag-str [x y dir]
  (let [[dx dy] (get directions dir)
        diag-elems (for [offs (range 0 4)
                         :let [c (get-char (dx x offs) (dy y offs))]]
                     c)]
    (apply str diag-elems)))

(defn count-occurrences [re line]
  (+
    (count (re-seq re line))
    (count (re-seq re (string/reverse line)))))

(defn indices-with [sought-char row]
  (->> row
       (map-indexed vector)
       (filter (fn [[_ c]] (= c sought-char)))
       (map first)))

(defn count-diag [x y]
  (->> (keys directions)
       (map #(get-diag-str x y %))
       (map #(count-occurrences xmas %))
       (reduce +)))

(defn diagonal-matches []
  (->> (map-indexed vector rows)
       (map (fn [[y row]]
              (map #(count-diag % y) (indices-with \X row))))
       (flatten)
       (reduce +)))

(defn part1 []
  (let [row-matches (->> rows
                         (map #(count-occurrences xmas %))
                         (reduce +))
        col-matches (->> cols
                         (map #(count-occurrences xmas %))
                         (reduce +))]
    (+ row-matches col-matches (diagonal-matches))))

;; Part 2

(defn right-diag [x y]
  (let [mid (get-char x y)
        right-down (get-char (+ x 1) (+ y 1))
        right-up (get-char (- x 1) (- y 1))]
    (str right-up mid right-down)))

(defn left-diag [x y]
  (let [mid (get-char x y)
        left-down (get-char (- x 1) (+ y 1))
        left-up (get-char (+ x 1) (- y 1))]
    (str left-up mid left-down)))

(defn has-x-mas [x y]
  (let [right (right-diag x y)
        left (left-diag x y)]
    (and
      (not= 0 (count-occurrences x-mas right))
      (not= 0 (count-occurrences x-mas left)))))

(defn part2 []
  (->> (map-indexed vector rows)
       (mapcat (fn [[y row]] (zipmap (indices-with \A row) (repeat y))))
       (filter (fn [[x y]] (has-x-mas x y)))
       (count)))