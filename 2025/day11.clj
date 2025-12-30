;advent-of-code-2025.day11
(ns day11
  (:require [clojure.string :as str]))

(def data (->> (slurp "2025/in11")
               str/split-lines
               (reduce (fn [dict line]
                         (let [[k & vs] (re-seq #"\w+" line)]
                           (assoc dict k (set vs))))
                       {})))

(def cal
  (memoize
   (fn [curr dst]
     (if (= curr dst)
       1
       (let [nxts (data curr)]
         (->> (map #(cal % dst) nxts)
              (reduce +)))))))

;; part 1
(cal "you" "out")

;; part 2
(*
 (cal "svr" "fft")
 (cal "fft" "dac")
 (cal "dac" "out"))