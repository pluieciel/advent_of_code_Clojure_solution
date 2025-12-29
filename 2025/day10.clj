;advent-of-code-2025.day10
(ns day10
  (:require [clojure.string :as str]
            [clojure.math.combinatorics :as comb]))

(def data (->> (slurp "2025/in10")
               str/split-lines
               (map (fn [line]
                      (->> (str/split line #" ")
                           (map #(re-seq #"\.|\#|\d+" %)))))))

(defn get-conf [[_ & b]]
  (->> (butlast b)
       (map #(map read-string %))
       comb/subsets
       (map (fn [bs]
              {(->> bs
                    (apply concat)
                    frequencies
                    (filter #(odd? (val %)))
                    keys
                    set)
               [bs]}))
       (apply merge-with concat)))

(defn get-target [[t & _]]
  (->> t
       (map-indexed vector)
       (filter #(= "#" (second %)))
       (map first)
       set))

;; part 1
(defn cal-one [line]
  (->> (get-target line)
       ((get-conf line))
       (map count)
       (apply min)))

(->> (map cal-one data)
     (reduce +))

;; part 2
;; ref: https://old.reddit.com/r/adventofcode/comments/1pk87hl/2025_day_10_part_2_bifurcate_your_way_to_victory/
(defn get-odd-pos [req]
  (->> req
       (map-indexed vector)
       (filter #(odd? (second %)))
       (map first)
       set))

(defn get-target2 [line]
  (->> (last line)
       (map read-string)))

(def cal-recur
  (memoize
   (fn [req conf]
     (cond
       (every? zero? req) 0
       (some neg? req) Integer/MAX_VALUE
       (every? even? req) (* 2 (cal-recur (map #(/ % 2) req) conf))
       :else
       (if-let [lst (conf (get-odd-pos req))]
         (reduce
          (fn [final bs]
            (let [newreq (reduce (fn [v id] (update v id dec)) (vec req) (apply concat bs))
                  res (+ (count bs)
                         (* 2 (cal-recur (map #(/ % 2) newreq) conf)))]
              (min final res)))
          Integer/MAX_VALUE
          lst)
         Integer/MAX_VALUE)))))

(defn cal-one2 [line]
  (cal-recur (get-target2 line) (get-conf line)))

(->> (map cal-one2 data)
     (reduce +))