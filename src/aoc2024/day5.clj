(ns aoc2024.day5
  (:gen-class))

(require '[clojure.string :as string]
         '[clojure.java.io :as io])

(def page-pair #"(\d+)\|(\d+)")
(defn is-pair [line]
  (re-matches page-pair line))
(def input (->> (string/split-lines (slurp (io/resource "day5.txt")))))
(def pairs (take-while #(is-pair %) input))
(def orderings (->> input
                    (reverse)
                    (take-while #(not (is-pair %)))
                    (filter #(not (string/blank? %)))
                    (map #(string/split % #","))
                    (map #(map Integer/parseInt %))))
(defn parse-pair [pair]
  (let [match (re-find page-pair pair)
        p1 (Integer/parseInt (second match))
        p2 (Integer/parseInt (last match))]
    [p1 p2]))

(def after (->> pairs
                (map parse-pair)
                (group-by first)
                (#(update-vals % (fn [p] (map second p))))))
(def before (->>
              (map parse-pair pairs)
              (group-by last)
              (#(update-vals % (fn [p] (map first p))))))

(defn middle-page [pages]
  (nth pages (quot (count pages) 2)))

(defn no-disallowed-pages-before? [set coll]
  (every? #(not-any? (fn [p] (= % p)) set) coll))

(defn is-in-order [ordering]
  (loop [remaining ordering]
    (if (empty? remaining)
      true
      (let [page (first remaining)
            allowed-before (get before page)
            is-ordered (no-disallowed-pages-before? allowed-before (rest remaining))]
        (if is-ordered
          (recur (rest remaining))
          false)))))

(defn part1 []
  (->> orderings
       (filter is-in-order)
       (map middle-page)
       (reduce +)))

;; Part 2
(defn count-afters [ordering elem]
  (let [afters (mapcat #(get after %) ordering)
        occurrences (count (filter #(= % elem) afters))]
    occurrences))

(defn after-occurrences [ordering]
  (reduce (fn [m page] (assoc m page (count-afters ordering page))) {} ordering))

(defn part2 []
  (let [incorrect-orders (filter (complement is-in-order) orderings)]
    (->> incorrect-orders
         (map #(after-occurrences %))
         (map #(sort-by second %))
         (map #(map first %))
         (map middle-page)
         (reduce +))))