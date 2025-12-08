;advent-of-code-2025.day08
(ns day08
  (:require [clojure.string :as str]
            [clojure.math.combinatorics :as comb]
            [clojure.set :as set]))

(def dots (->> (slurp "2025/in08")
               str/split-lines
               (map #(map read-string (re-seq #"\d+" %)))))

(defn dist2 [[x1 y1 z1] [x2 y2 z2]]
  (let [dx (- x1 x2) dy (- y1 y2) dz (- z1 z2)]
    (transduce (map #(* % %)) + [dx dy dz])))

(def todo (->> (comb/combinations dots 2)
               (sort-by #(apply dist2 %))))

;; part 1, not efficient
(->> (reduce
      (fn [dict [dot1 dot2]]
        (if ((get dict dot1) dot2)
          dict
          (let [s1 (get dict dot1)
                s2 (get dict dot2)
                s12 (set/union s1 s2)]
            (reduce #(assoc %1 %2 s12) dict s12))))
      (into {} (map #(vector % #{%}) dots))
      (take 1000 todo))
     vals
     distinct
     (map count)
     (sort-by #(- %))
     (take 3)
     (reduce *))

;; part 2, not efficient
(reduce
 (fn [dict [[x1 _ _ :as dot1] [x2 _ _ :as dot2]]]
   (if ((get dict dot1) dot2)
     dict
     (let [s1 (get dict dot1)
           s2 (get dict dot2)
           s12 (set/union s1 s2)]
       (if (= (count dots) (count s12))
         (reduced (* x1 x2))
         (reduce #(assoc %1 %2 s12) dict s12)))))
 (into {} (map #(vector % #{%}) dots))
 todo)