(ns aoc2021.day03
  (:require [clojure.string :as str]))


(defn parse-input-file [path]
  (->> path
       slurp
       str/split-lines))

(defn ainc [arr idx]
  (aset-int arr idx (inc (aget arr idx))))

(defn inc-ones-counters [counter-arr s]
  (dotimes [i (alength counter-arr)]
    (when (= (nth s i) \1)
      (ainc counter-arr i))))

(defn count-ones [ss]
  (when (seq ss)
    (let [counters (int-array (-> ss first count))]
      (doseq [s ss]
        (inc-ones-counters counters s))
      counters)))

(defn counters->gamma [counters]
  (reduce (fn [x v] (+ (* 2 x)
                       (if (< v 500) 0 1)))
          0
          counters))

(defn counters->epsilon [counters]
  (reduce (fn [x v] (+ (* 2 x)
                       (if (< v 500) 1 0)))
          0
          counters))

(defn filter-oxygen-bit-criteria [i data]
  (let [counters (count-ones data)
        half (/ (count data) 2)]
    (if (<= half (aget counters i))
      (filter #(= \1 (nth % i)) data)
      (filter #(= \0 (nth % i)) data))))

(defn filter-co2-bit-criteria [i data]
  (let [counters (count-ones data)
        half (/ (count data) 2)]
    (if (< (aget counters i) half)
      (filter #(= \1 (nth % i)) data)
      (filter #(= \0 (nth % i)) data))))

(defn calculate-rating [filter-f data]
  (loop [i 0 data data]
    (let [num-left (count data)]
      (if (= 1 num-left)
        (Integer/parseInt (first data) 2)
        (recur (inc i) (filter-f i data))))))

(defn oxygen-gen-rating [data]
  (calculate-rating filter-oxygen-bit-criteria data))

(defn co2-scrub-rating [data]
  (calculate-rating filter-co2-bit-criteria data))

(defn puzzle-1 []
  (let [data (parse-input-file "./resources/day03.input")
        counters (count-ones data)
        gamma-rate (counters->gamma counters)
        epsilon-rate (counters->epsilon counters)]
    (* gamma-rate epsilon-rate)))

(defn puzzle-2 []
  (let [data (parse-input-file "./resources/day03.input")
        oxy (oxygen-gen-rating data)
        co2 (co2-scrub-rating data)]
    (* oxy co2)))

(comment
  (puzzle-1)
  (puzzle-2))
