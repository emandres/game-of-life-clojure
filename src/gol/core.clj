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

(defn next-round
  [board]
  (let [cells (apply clojure.set/union
                     (cons board (map neighbors board)))]
    (set (filter #(alive-next-round? % board) cells))))
