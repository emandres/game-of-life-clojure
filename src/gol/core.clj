(ns gol.core)

(defn make-board
  [& alive-cells]
  (set alive-cells))

(defn alive?
  [cell alive-cells]
  (alive-cells cell))

(defn neighbors
  [[x y]]
  (set (for [dx (range -1 2)
             dy (range -1 2)
             :when (not= dx dy 0)]
         [(+ x dx) (+ y dy)])))

(defn alive-next-round?
  [cell board]
  (let [neighbor-count (count (filter #(alive? % board) 
                                      (neighbors cell)))]
    (if (alive? cell board)
      (#{2 3} neighbor-count)
      (= 3 neighbor-count))))

(defn min-and-max-by
  [f coll]
  (apply (juxt min max)
         (map f coll)))

(defn board-range
  [axis board]
  (let [axis-fn (axis {:x first :y last})]
    (apply range (map + (min-and-max-by axis-fn board) [-1 2]))))

(defn next-round
  [board]
  (let [x-range (board-range :x board)
        y-range (board-range :y board)]
    (set (for [x x-range
               y y-range
               :when (alive-next-round? [x y] board)]
           [x y]))))
