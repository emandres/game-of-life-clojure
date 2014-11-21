(ns gol.core-test
  (:require [clojure.test :refer :all]
            [gol.core :refer :all]))

(deftest board-is-represented-as-a-set-of-points
  (is (= #{} (make-board)))
  (is (= #{[1 1] (make-board [1 1])})))

(deftest cell-is-alive-when-contained-in-the-set
  (let [board (make-board [1 1])]
    (is (alive? [1 1] board))
    (is (not (alive? [0 0] board)))))

(deftest cells-have-neighbors-located-at-the-edges-of-the-three-by-three-surrounding-grid
  (let [neighbors (neighbors [1 1])]
    (doall (map #(is (neighbors %))
                [[0 0] [1 0] [2 0]
                 [0 1]       [2 1]
                 [0 2] [1 2] [2 2]]))))

(deftest alive-cells-with-fewer-than-two-neighbors-die
  (is (not (alive-next-round? [1 1] (make-board [1 1]))))
  (is (not (alive-next-round? [1 1] (make-board [1 1] [0 0])))))

(deftest alive-cells-with-two-or-three-neighbors-survive
  (is (alive-next-round? [1 1] (make-board [1 1] [0 0] [0 1])))
  (is (alive-next-round? [1 1] (make-board [1 1] [0 0] [0 1] [0 2]))))

(deftest alive-cells-with-more-than-three-neighbors-die
  (is (not (alive-next-round? [1 1] (make-board [1 1] [0 0] [0 1] [0 2] [1 2]))))
  (is (not (alive-next-round? [1 1] (make-board [1 1] [0 0] [0 1] [0 2] [1 2] [2 0]))))
  (is (not (alive-next-round? [1 1] (make-board [1 1] [0 0] [0 1] [0 2] [1 2] [2 0] [2 1]))))
  (is (not (alive-next-round? [1 1] (make-board [1 1] [0 0] [0 1] [0 2] [1 2] [2 0] [2 1] [1 0]))))
  (is (not (alive-next-round? [1 1] (apply make-board (neighbors [1 1]))))))

(deftest dead-cells-with-exactly-three-neighbors-are-reborn
  (is (alive-next-round? [1 1] (make-board [0 0] [1 0] [2 0])))
  (is (not (alive-next-round? [1 1] (make-board))))
  (is (not (alive-next-round? [1 1] (apply make-board [1 1] (neighbors [1 1]))))))

(deftest some-cell-patterns-are-stable
  (let [pattern #{[0 0] [0 1] [1 0] [1 1]}]
    (is (= pattern (next-round pattern))))
  (let [pattern #{[1 0] [0 1] [2 1] [1 2]}]
    (is (= pattern (next-round pattern))))
  (let [pattern #{[1 0] [0 1] [2 1] [0 2] [2 2] [1 3]}]
    (is (= pattern (next-round pattern)))))

(deftest dead-cells-must-be-checked-for-the-next-round
  (is (= #{[0 0] [1 0] [0 1] [1 1]} (next-round #{[0 0] [1 0] [0 1]}))))
