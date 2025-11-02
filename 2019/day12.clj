;advent-of-code-2019.day12
(ns day12)

(defn parse [file]
  (->> (slurp file)
       (re-seq #"-?[0-9]+")
       (map read-string)
       (partition 3)
       (mapv vec)))

(def init-pos (parse "2019/in12"))
(def init-vel (vec (repeat 4 [0 0 0])))

(defn get-dx [x xs]
  (let [bigger (count (filter #(> % x) xs))
        smaller (count (filter #(< % x) xs))]
    (- bigger smaller)))

(defn step [[pos vel]]
  (let [axis (apply map vector pos)
        dv (for [xyz pos]
             (mapv get-dx xyz axis))
        new-vel (mapv #(mapv + %1 %2) vel dv)]
    [(mapv #(mapv + %1 %2) pos new-vel)
     new-vel]))

;; part1
(->> (iterate step [init-pos init-vel])
     (#(nth % 1000))
     (map (fn [ps] (map #(apply + (map Math/abs %)) ps)))
     (apply map *)
     (apply +))

;; part2
(def init-state [init-pos init-vel])

(defn gcd [a b]
  (if (zero? b)
    a
    (recur b (mod a b))))

(defn lcm [a b]
  (/ (* a b) (gcd a b)))

(loop [state init-state
       cnt 0
       dict {}]
  (let [new-state (step state)
        new-cnt (inc cnt)
        new-dict (-> dict
                     (#(let [dict %]
                         (if (or (dict :x) (not= (map first (apply concat init-state)) (map first (apply concat new-state))))
                           dict
                           (assoc dict :x new-cnt))))
                     (#(let [dict %]
                         (if (or (dict :y) (not= (map second (apply concat init-state)) (map second (apply concat new-state))))
                           dict
                           (assoc dict :y new-cnt))))
                     (#(let [dict %]
                         (if (or (dict :z) (not= (map last (apply concat init-state)) (map last (apply concat new-state))))
                           dict
                           (assoc dict :z new-cnt)))))]
    (if (< (count new-dict) 3)
      (recur new-state new-cnt new-dict)
      (->> (vals new-dict)
           (reduce lcm)))))