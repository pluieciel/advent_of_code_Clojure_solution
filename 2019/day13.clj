;advent-of-code-2019.day13
(ns day13
  (:require [clojure.string :as str]))

;; from day09
(def program
  (->> (str/split (slurp "2019/in13") #",")
       (map read-string)
       (map vector (range))
       (into {})))

(defn getmode [pos idx modes dict rbase]
  (case (get modes idx 0)
    0 (get dict (+ pos (inc idx)) 0)
    1 (+ pos (inc idx))
    2 (+ rbase (get dict (+ pos (inc idx)) 0))))

(defn calone [op dict pos modes rbase]
  (assoc dict
         (getmode pos 2 modes dict rbase)
         (op (get dict (getmode pos 0 modes dict rbase) 0)
             (get dict (getmode pos 1 modes dict rbase) 0))))

(defn calcmp [op dict pos modes rbase]
  (assoc dict
         (getmode pos 2 modes dict rbase)
         (if (op (get dict (getmode pos 0 modes dict rbase) 0)
                 (get dict (getmode pos 1 modes dict rbase) 0))
           1 0)))

(defn jump [dict pos modes rbase f]
  (let [arg (get dict (getmode pos 0 modes dict rbase) 0)]
    (if (f arg)
      (+ pos 3)
      (get dict (getmode pos 1 modes dict rbase) 0))))

(defn cal [dict pos rbase output xy-t]
  (if (or (= 99 (get dict pos))
          (xy-t :final))
    xy-t
    (let [op-long (get dict pos)
          [op & modes] (->> op-long
                            str
                            (re-seq #"\d\d$|\d")
                            reverse
                            (map Integer/parseInt))
          modes (vec modes)]
      (case op
        1 (cal (calone + dict pos modes rbase) (+ pos 4) rbase output xy-t)
        2 (cal (calone * dict pos modes rbase) (+ pos 4) rbase output xy-t)
        3 (let [ball-x (ffirst (first (filter #(= 4 (val %)) xy-t)))
                pad-x (ffirst (first (filter #(= 3 (val %)) xy-t)))
                move (cond (< pad-x ball-x) 1 (> pad-x ball-x) -1 :else 0)]
            (cal (assoc dict (getmode pos 0 modes dict rbase) move) (+ pos 2) rbase output xy-t))
        4 (let [output (conj output (get dict (getmode pos 0 modes dict rbase) 0))]
            (if (= 3 (count output))
              (let [k (butlast output)
                    v (last output)
                    new-xy-t (assoc xy-t k v)]
                (cal dict (+ pos 2) rbase [] (if (and (= k '(-1 0))
                                                      (empty? (filter #(= 2 (val %)) new-xy-t))) 
                                               (assoc new-xy-t :final 1)
                                               new-xy-t)))
              (cal dict (+ pos 2) rbase output xy-t)))
        5 (cal dict (jump dict pos modes rbase zero?) rbase output xy-t)
        6 (cal dict (jump dict pos modes rbase #(not (zero? %))) rbase output xy-t)
        7 (cal (calcmp < dict pos modes rbase) (+ pos 4) rbase output xy-t)
        8 (cal (calcmp = dict pos modes rbase) (+ pos 4) rbase output xy-t)
        9 (cal dict (+ pos 2) (+ rbase (get dict (getmode pos 0 modes dict rbase) 0)) output xy-t)))))

;; part1
(->> (cal program 0 0 [] {})
     (filter #(= 2 (val %)))
     count)

;; part2
(->> (cal (assoc program 0 2) 0 0 [] {})
     (filter #(= '(-1 0) (key %))))

;; ;; print and check map
;; (let [output (cal program 0 0 [] {})
;;       dict (->> output
;;                 (into {}))
;;       axis (keys dict)
;;       max-x (apply max (map first axis))
;;       max-y (apply max (map second axis))]
;;   (for [y (range (inc max-y))]
;;     (apply str
;;            (for [x (range (inc max-x))]
;;              (case (dict [x y])
;;                0 " "
;;                1 "#"
;;                2 "O"
;;                3 "_"
;;                4 ".")))))

;; ("##########################################"
;;  "#                                        #"
;;  "# OOOOO O OO   OO  O O OO O OO O OOOO O  #"
;;  "#    OOOOOOOO OOO O  O OO   OOO   O    O #"
;;  "#  OOO  O O  O  OO OO   OOO    O   O OO  #"
;;  "#   O O        OOOO  OO  OOO   O    OO O #"
;;  "# O  O  OO  O  OO O  O OO O OO O OO      #"
;;  "# OO        O O     OOOOO  O O  OOOO     #"
;;  "# O OO OO  O O O    O OOO O  O     O     #"
;;  "#   O O  O O     O OO   OO  OOOOO OO O O #"
;;  "# O    O  OO O    O OO    OOOOO O O O  O #"
;;  "#    O O   OO  OO O     O O  O OOO  O  O #"
;;  "# OO OO  OO OOOOO O     OO   O         O #"
;;  "#   OO OOO O OO   OOOOOO    O OO     OO  #"
;;  "#                                        #"
;;  "#                  .                     #"
;;  "#                                        #"
;;  "#                                        #"
;;  "#                    _                   #"
;;  "#                                        #")