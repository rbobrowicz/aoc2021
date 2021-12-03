(ns aoc2021.day02
  (:require [clojure.string :as str]))

(defn parse-line [line]
  (let [[dir amount] (str/split line #" ")]
    [(keyword dir) (Integer/parseInt amount)]))

(defn parse-input-file [path]
  (->> path
       slurp
       str/split-lines
       (map parse-line)))

(defn calculate-position [course]
  (loop [horizontal 0
         depth 0
         cs course]
    (if-let [[[dir magnitude] & rst] cs]
      (case dir
        :forward (recur (+ horizontal magnitude) depth rst)
        :up (recur horizontal (- depth magnitude) rst)
        :down (recur horizontal (+ depth magnitude) rst))
      {:horizontal horizontal
       :depth depth})))

(defn calculate-position-2 [course]
  (loop [horizontal 0
         depth 0
         aim 0
         cs course]
    (if-let [[[dir magnitude] & rst] cs]
      (case dir
        :forward (recur (+ horizontal magnitude) (+ depth (* aim magnitude)) aim rst)
        :up (recur horizontal depth (- aim magnitude) rst)
        :down (recur horizontal depth (+ aim magnitude) rst))
      {:horizontal horizontal
       :depth depth
       :aim aim})))

(defn puzzle-1 []
  (let [data (parse-input-file "./resources/day02.input")
        {depth :depth
         horizontal :horizontal} (calculate-position data)]
    (* depth horizontal)))

(defn puzzle-2 []
  (let [data (parse-input-file "./resources/day02.input")
        {depth :depth
         horizontal :horizontal} (calculate-position-2 data)]
    (* depth horizontal)))

(comment
  (puzzle-1)
  (puzzle-2))
