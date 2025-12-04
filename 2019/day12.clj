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
        op (fn [dict [key func]]
             (if (or (dict key) (apply not= (map #(map func (apply concat %)) [init-state new-state])))
               dict
               (assoc dict key new-cnt)))
        new-dict (reduce op dict [[:x first] [:y second] [:z last]])]
    (if (< (count new-dict) 3)
      (recur new-state new-cnt new-dict)
      (->> (vals new-dict)
           (reduce lcm)))))