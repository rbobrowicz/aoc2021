(ns aoc2021.day01
  (:require [clojure.string :as str]))

(defn read-puzzle-input [fname]
  (->> fname
       slurp
       str/split-lines
       (map #(Integer/parseInt %))))

(defn count-step-rises [data]
  (->> data
       (partition 2 1)
       (map (partial apply <))
       (filter true?)
       count))

(defn partition-sum [step data]
  (->> data
       (partition step 1 data)
       (map (partial apply +))))

(defn puzzle-1 []
  (let [data (read-puzzle-input "./resources/day01.input")]
    (count-step-rises data)))

(defn puzzle-2 []
  (let [data (read-puzzle-input "./resources/day01.input")]
    (count-step-rises
     (partition-sum 3 data))))

(comment
  (puzzle-1)
  (puzzle-2))
