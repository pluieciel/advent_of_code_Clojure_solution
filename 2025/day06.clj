;advent-of-code-2025.day06
(ns day06
  (:require [clojure.string :as str]))

(def rawdata (->> (slurp "2025/in06")
                  str/split-lines))
(def data (->> rawdata
               (map #(map read-string (re-seq #"\d+|\*|\+" %)))))
(def nums (butlast data))
(def ops (last data))

;; part 1
(->> (apply map list nums)
     (map #(apply (resolve %1) %2) ops)
     (reduce +))

;; part 2
(defn spaces? [s] (every? #(= \space %) s))

(->> rawdata
     butlast
     (apply map str)
     (partition-by spaces?)
     (remove #(spaces? (first %)))
     (map #(apply (resolve %1) (map read-string %2)) ops)
     (reduce +))