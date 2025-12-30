;advent-of-code-2025.day12
(ns day12
  (:require [clojure.string :as str]))

(def data (str/split (slurp "2025/in12") #"\n\n"))

(def patterns (->> (butlast data)
                   (map #(-> (frequencies %)
                             (get \#)))
                   (vec)))

(->> (last data)
     str/split-lines
     (filter (fn [line]
               (let [[a b & lst] (map read-string (re-seq #"\d+" line))]
                 (>= (* a b) (reduce + (map * lst patterns))))))
     count)