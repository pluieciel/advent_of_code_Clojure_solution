;advent-of-code-2025.day07
(ns day07
  (:require [clojure.string :as str]))

(def data (->> (slurp "2025/in07")
               str/split-lines
               (partition 2)
               (map #(vec (first %)))))

(def start (.indexOf (first data) \S))
(def positions (rest data))

(let [[dict cnt] (reduce
                  (fn [[curr cnt] pos]
                    (let [after (map
                                 (fn [[p t]]
                                   (if (= \^ (pos p))
                                     {(dec p) t (inc p) t}
                                     {p t}))
                                 curr)]
                      [(apply merge-with + after)
                       (+ cnt (count (filter #(= 2 (count %)) after)))]))
                  [{start 1} 0]
                  positions)]
  ;; part 1
  cnt

  ;; part 2
  (reduce + (vals dict)))