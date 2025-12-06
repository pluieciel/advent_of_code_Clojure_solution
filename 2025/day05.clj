;advent-of-code-2025.day05
(ns day05
  (:require [clojure.string :as str]))

(let [[fresh todo] (str/split (slurp "2025/in05") #"\n\n")]
  (def fresh (->> fresh
                  str/split-lines
                  (map #(map read-string (re-seq #"\d+" %)))))
  (def todo (->> todo
                 str/split-lines
                 (map read-string))))

;; part 1
(defn check-one [n]
  (some (fn [[s e]] (<= s n e)) fresh))

(->> (keep check-one todo)
     count)

;; part 2
(->> (sort-by first fresh)
     ((fn [[fst & res]]
        (reduce
         (fn [acc [ns ne :as new]]
           (let [done (rest acc) [s e] (first acc)]
             (cond
               (< (inc e) ns) (conj acc new)
               (<= ne e) acc
               :else (conj done (list s ne)))))
         (list fst)
         res)))
     (map (fn [[s e]] (inc (- e s))))
     (reduce +))