;advent-of-code-2025.day01
(ns day01
  (:require [clojure.string :as str]))

(defn parse [input]
  (->> (slurp input)
       str/split-lines
       (map (fn [[d & n]] [({\L - \R +} d) (read-string (apply str n))]))))

(def moves (parse "2025/in01"))

;; part 1
(reduce
 (fn [[pos cnt] [f m]]
   (let [npos (mod (f pos m) 100)]
     [npos (if (zero? npos) (inc cnt) cnt)]))
 [50 0]
 moves)

;; part 2
(reduce
 (fn [[pos cnt] [f m]]
   (let [tmp (f pos m)
         q (quot tmp 100)
         npos (mod tmp 100)]
     (cond
       (zero? tmp) [npos (inc cnt)]
       (pos? tmp) [npos (+ cnt q)]
       (neg? tmp) [npos (+ cnt (- q) (if (zero? pos) 0 1))])))
 [50 0]
 moves)