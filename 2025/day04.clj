;advent-of-code-2025.day04
(ns day04
  (:require [clojure.string :as str]))

(defn parse [input]
  (->> (slurp input)
       str/split-lines
       (mapv vec)))

(def pos (parse "2025/in04"))
(def w (count (first pos)))
(def h (count pos))
(def neig (for [x (range -1 2) y (range -1 2) :when (not (= [x y] [0 0]))] [x y]))

;; part 1
(defn ok? [y x]
  (->> (map #(mapv + [y x] %) neig)
       (map #(get-in pos %))
       (filter #(= \@ %))
       count
       (#(< % 4))))

(->> (for [x (range w) y (range h)
           :when (and (= \@ (get-in pos [y x])) (ok? y x))]
       [y x])
     count)

;; part 2
(defn ok2? [y x done]
  (->> (map #(mapv + [y x] %) neig)
       (map #(if (done %) \. (get-in pos %)))
       (filter #(= \@ %))
       count
       (#(< % 4))))

(loop [done #{}]
  (let [todo (for [x (range w) y (range h)
                   :when (and (not (done [y x])) (= \@ (get-in pos [y x])) (ok2? y x done))]
               [y x])]
    (if (empty? todo)
      (count done)
      (recur (into done todo)))))