;advent-of-code-2025.day03
(ns day03
  (:require [clojure.string :as str]))

(defn parse [input]
  (->> (slurp input)
       str/split-lines
       (map #(map read-string (re-seq #"\d" %)))))

(defn cal [cal-line]
  (->> (parse "2025/in03")
       (map cal-line)
       (reduce +)))

;; part 1
(defn cal-line1 [lst]
  (->> (reduce
        (fn [[a b] new]
          (cond
            (< a b) [b new]
            (< b new) [a new]
            :else [a b]))
        [0 0] lst)
       (map * [10 1])
       (apply +)))

(cal cal-line1)

;; part 2
(defn cal-line2 [lst]
  (->> (reduce
        (fn [curr new]
          (->> (concat curr [new 10])
               (partition 2 1)
               ((fn [pairs]
                  (loop [done [] todo pairs]
                    (if (apply < (first todo))
                      (concat done (rest todo))
                      (recur (conj done (first todo)) (rest todo))))))
               (map first)))
        (repeat 12 0) lst)
       (apply str)
       read-string))

(cal cal-line2)