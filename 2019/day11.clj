;advent-of-code-2019.day11
(ns day11
  (:require [clojure.string :as str]))

;; from day09
(def program
  (->> (str/split (slurp "2019/in11") #",")
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

;; new code
(def dir [[0 1] [1 0] [0 -1] [-1 0]])

(defn cal [dict pos rbase paintmap outflag robot-pos robot-dir-idx]
  (if (= 99 (get dict pos))
    paintmap
    (let [op-long (get dict pos)
          [op & modes] (->> op-long
                            str
                            (re-seq #"\d\d$|\d")
                            reverse
                            (map Integer/parseInt))
          modes (vec modes)]
      (case op
        1 (cal (calone + dict pos modes rbase) (+ pos 4) rbase paintmap outflag robot-pos robot-dir-idx)
        2 (cal (calone * dict pos modes rbase) (+ pos 4) rbase paintmap outflag robot-pos robot-dir-idx)
        3 (cal (assoc dict (getmode pos 0 modes dict rbase) (get paintmap robot-pos 0)) (+ pos 2) rbase paintmap outflag robot-pos robot-dir-idx)
        4 (let [output (get dict (getmode pos 0 modes dict rbase) 0)] 
            (if (= outflag :first)
              (cal dict (+ pos 2) rbase (assoc paintmap robot-pos output) :second robot-pos robot-dir-idx)
              (let [new-dir-idx (mod (+ robot-dir-idx (dec (* 2 output))) 4)]
                (cal dict (+ pos 2) rbase paintmap :first (mapv + robot-pos (dir new-dir-idx)) new-dir-idx))))
        5 (cal dict (jump dict pos modes rbase zero?) rbase paintmap outflag robot-pos robot-dir-idx)
        6 (cal dict (jump dict pos modes rbase #(not (zero? %))) rbase paintmap outflag robot-pos robot-dir-idx)
        7 (cal (calcmp < dict pos modes rbase) (+ pos 4) rbase paintmap outflag robot-pos robot-dir-idx)
        8 (cal (calcmp = dict pos modes rbase) (+ pos 4) rbase paintmap outflag robot-pos robot-dir-idx)
        9 (cal dict (+ pos 2) (+ rbase (get dict (getmode pos 0 modes dict rbase) 0)) paintmap outflag robot-pos robot-dir-idx)))))

;; part 1
(->> (cal program 0 0 {} :first [0 0] 0)
     count)

;; part 2
(let [whites (->> (cal program 0 0 {[0 0] 1} :first [0 0] 0)
                  (filter #(= 1 (val %)))
                  keys)
      xs (map first whites)
      ys (map second whites)
      min-x (apply min xs)
      min-y (apply min ys)
      max-x (apply max xs)
      max-y (apply max ys)]
  (for [y (range max-y (dec min-y) -1)]
    (apply str
           (for [x (range min-x (inc max-x))]
             (if ((set whites) [x y]) "#" " ")))))