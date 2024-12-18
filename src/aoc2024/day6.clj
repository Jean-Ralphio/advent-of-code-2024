(ns aoc2024.day6
  (:gen-class))

(require '[clojure.string :as string]
         '[clojure.java.io :as io]
         '[clojure.set :as cset])

(def rows (->> (string/split-lines (slurp (io/resource "day6.txt")))))
(def input (string/join rows))
(def row-length (count (first rows)))

(def step {:up    (- row-length)
           :down  row-length
           :left  -1
           :right 1})

(defn next-index
  ([idx direction]
   (next-index idx direction input))
  ([idx direction lab]
   (let [next-idx (+ idx (step direction))
         valid? (case direction
                  :up (> next-idx 0)
                  :down (< next-idx (count lab))
                  :left (pos? (mod idx row-length))
                  :right (pos? (mod next-idx row-length)))]
     (if valid? next-idx -1))))

(defn next-dir [current-dir]
  (case current-dir
    :up :right
    :down :left
    :left :up
    :right :down))

(defn predict-positions
  ([idx]
   (predict-positions idx :up))
  ([idx direction]
   (loop [dir direction
          curr-idx idx
          visited #{}]
     (let [next-idx (next-index curr-idx dir)
           next-char (get input next-idx)]
       (if (nil? next-char)
         (conj visited curr-idx)
         (if (= \# next-char)
           (recur (next-dir dir) curr-idx (conj visited curr-idx))
           (recur dir next-idx (conj visited curr-idx))))))))

(defn part1 []
  (let [m (re-matcher #"\^" input)
        start (re-find m)]
    (count (predict-positions (.start m)))))

;; Part 2

(defn has-loop
  ([idx lab]
   (has-loop idx :up lab))
  ([idx direction lab]
   (loop [dir direction
          curr-idx idx
          visited #{}]
     (let [next-idx (next-index curr-idx dir lab)
           next-char (get lab next-idx)
           pos [curr-idx dir]]
       (if (contains? visited pos)
         true
         (if (nil? next-char)
           false
           (if (= \# next-char)
             (recur (next-dir dir) curr-idx (conj visited pos))
             (recur dir next-idx (conj visited pos)))))))))

(defn part2 []
  (let [m (re-matcher #"\^" input)
        f (re-find m)
        start (.start m)
        positions (cset/difference (predict-positions start) #{start})
        ; how elegant
        possible-loops (map #(str (subs input 0 %) "#" (subs input (inc %)) positions))]
    (count (filter #(has-loop start %) possible-loops))))