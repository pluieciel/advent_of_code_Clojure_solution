;advent-of-code-2025.day09
(ns day09
  (:require [clojure.string :as str]
            [clojure.math.combinatorics :as comb]))

(def dots (->> (slurp "2025/in09")
               str/split-lines
               (map #(map read-string (re-seq #"\d+" %)))))

;; part 1
(->> (comb/combinations dots 2)
     (map (fn [pair]
            (->> pair
                 (apply map -)
                 (map #(inc (Math/abs %)))
                 (apply *))))
     (apply max))

;; part 2
(def borders (partition 2 1 (conj (vec dots) (first dots))))

(defn overlap [[[x1 y1] [x2 y2]] [[x3 y3] [x4 y4]]]
  (let [[x1 x2] (sort [x1 x2])
        [y1 y2] (sort [y1 y2])
        [x3 x4] (sort [x3 x4])
        [y3 y4] (sort [y3 y4])]
    (and (< x1 x4) (> x2 x3) (< y1 y4) (> y2 y3))))

(->> (comb/combinations dots 2)
     (remove (fn [pair]
               (some (fn [border] (overlap pair border)) borders)))
     (map (fn [pair]
            (->> pair
                 (apply map -)
                 (map #(inc (Math/abs %)))
                 (apply *))))
     (apply max))