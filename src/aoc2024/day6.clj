(ns aoc2024.day6
  (:require [clojure.string :as string]
            [clojure.java.io :as io]))

(def rows (->> (string/split-lines (slurp (io/resource "day6.txt")))))
(def input (string/join rows))
(def row-length (count (first rows)))

(def step {:up    (- row-length)
           :down  row-length
           :left  -1
           :right 1})

(def next-dir {:up    :right
               :right :down
               :down  :left
               :left  :up})

(defn next-index
  [idx direction]
  (let [next-idx (+ idx (step direction))
        within-bounds? (case direction
                         :up (> next-idx 0)
                         :down (< next-idx (count input))
                         :left (pos? (mod idx row-length))
                         :right (pos? (mod next-idx row-length)))]
    (if within-bounds? next-idx -1)))

(defn predict-positions
  ([idx]
   (predict-positions idx :up))
  ([idx direction]
   (loop [dir direction
          curr-idx idx
          visited #{}]
     (let [next-idx (next-index curr-idx dir)
           next-char (get input next-idx)]
       (cond
         (nil? next-char) (conj visited curr-idx)
         (= \# next-char) (recur (next-dir dir) curr-idx (conj visited curr-idx))
         :else (recur dir next-idx (conj visited curr-idx)))))))

(defn part1 []
  (let [start (string/index-of input "^")]
    (count (predict-positions start))))

;; Part 2

(defn has-loop
  ([idx lab]
   (has-loop idx :up lab))
  ([idx direction lab]
   (loop [dir direction
          curr-idx idx
          visited #{}]
     (let [next-idx (next-index curr-idx dir)
           next-char (get lab next-idx)
           pos [curr-idx dir]]
       (cond
         (contains? visited pos) true
         (nil? next-char) false
         (= \# next-char) (recur (next-dir dir) curr-idx (conj visited pos))
         :else (recur dir next-idx (conj visited pos)))))))

(defn part2 []
  (let [start (string/index-of input "^")
        positions (disj (predict-positions start) start)
        ; how elegant
        possible-loops (for [pos positions]
                         (str (subs input 0 pos) "#" (subs input (inc pos))))]
    (count (filter #(has-loop start %) possible-loops))))