;advent-of-code-2025.day02
(ns day02
  (:require [clojure.string :as str]))

(defn parse [input]
  (->> (str/split (slurp input) #",")
       (map #(map read-string (re-seq #"\d+" %)))))

(defn cal [pre?]
  (->> (parse "2025/in02")
     (mapcat
      (fn [[s e]]
        (->> (range s (inc e))
             (filter pre?))))
     (reduce +)))

;; part 1
(defn twice? [n]
  (let [st (str n)
        len (count st)]
    (and
     (even? len)
     (= (subs st 0 (/ len 2)) (subs st (/ len 2))))))

(cal twice?)

;; part 2
(defn rep? [n]
  (let [st (str n)
        len (count st)]
    (->> (range 1 (inc (quot len 2)))
         (some #(apply = (partition-all % st))))))

(cal rep?)