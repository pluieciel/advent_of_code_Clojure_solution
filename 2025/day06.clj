;advent-of-code-2025.day06
(ns day06
  (:require [clojure.string :as str]))

(def data (->> (slurp "2025/in06")
               str/split-lines
               (map #(map read-string (re-seq #"\d+|\*|\+" %)))))
(def nums (butlast data))
(def ops (->> (last data)
              (map resolve)))

;; part 1
(->> (apply map list nums)
     (map #(apply %1 %2) ops)
     (reduce +))

;; part 2
(def rawdata (->> (slurp "2025/in06")
                  str/split-lines
                  butlast))
(defn spaces? [s] (every? #(= \space %) s))

(->> rawdata
     (apply map str)
     (partition-by spaces?)
     (remove #(spaces? (first %)))
     (map #(apply %1 (map read-string %2)) ops)
     (reduce +))