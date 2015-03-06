(ns slide.game)

(defn game-over? [s]
  (empty? (:goals s)))


