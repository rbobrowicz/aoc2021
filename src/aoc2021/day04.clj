(ns aoc2021.day04
  (:require [clojure.string :as str]))

(defn parse-board [board-str]
  (->> #"\s"
       (str/split board-str)
       (filter #(not= "" %))
       (map #(Integer/parseInt %))
       vec))

(defn parse-input [lines]
  (let [[num-line boards-lines] (str/split lines #"\n" 2)
        nums (map #(Integer/parseInt %)
                  (str/split num-line #","))
        board-strs (map str/trim
                        (str/split boards-lines #"\n\n"))
        boards (map parse-board board-strs)]
    {:numbers nums
     :boards boards}))

(defn read-input-file [path]
  (->> path
       slurp
       parse-input))

(defn mark-number [n board]
  (replace {n :x} board))

(defn transpose-board [board]
  (for [i (range 0 5)
        j (range 0 5)]
    (nth board (+ i (* 5 j)))))

(defn winner? [board]
  (let [check-fn (fn [row] (every? #(= :x %) row))]
    (or
     (some check-fn (partition 5 board))
     (some check-fn (partition 5 (transpose-board board))))))

(defn play-bingo [{numbers :numbers boards :boards}]
  (let [steps (reductions (fn [state n]
                            {:called n
                             :boards
                             (map (fn [board] (mark-number n board)) (:boards state))})
                          {:called nil
                           :boards boards}
                          numbers)
        check-fn (fn [{called :called boards :boards}]
                   (when-let [s (seq (filter winner? boards))]
                     {:called called
                      :winner (first s)}))]
    (some check-fn steps)))

(defn play-anti-bingo [{numbers :numbers boards :boards}]
  (let [steps (reductions (fn [state n]
                            {:called n
                             :boards
                             (filter (complement winner?)
                                     (map (fn [board] (mark-number n board)) (:boards state)))})
                          {:called nil
                           :boards boards}
                          numbers)
        check-fn (fn [{called :called boards :boards}]
                   (when (= 1 (count boards))
                     {:remaining (rest (drop-while #(not= called %) numbers))
                      :called called
                      :loser (first boards)}))]
    (some check-fn steps)))

(defn puzzle-1 []
  (let [{called :called
         winner :winner}
        (play-bingo (read-input-file "./resources/day04.input"))
        remaining-nums (filter int? winner)
        sum-remaining (reduce + remaining-nums)]
    (* called sum-remaining)))

(defn puzzle-2 []
  (let [{loser :loser
         remaining :remaining}
        (play-anti-bingo (read-input-file "./resources/day04.input"))

        {winner :winner
         called :called}
        (play-bingo {:numbers remaining :boards [loser]})

        remaining-nums (filter int? winner)
        sum-remaining (reduce + remaining-nums)]
    (* called sum-remaining)))

(comment
  (puzzle-1)
  (puzzle-2))
