(ns slide.solve
  (:require [slide.move :as m]
            [slide.game :as g]))

;; BFS tracking states which have already been seen
(defn solve [s]
  (loop [q (into clojure.lang.PersistentQueue/EMPTY [[s []]])
         seen #{}]
    (when (seq q)
      (let [curr (peek q)
            [curr-state curr-path] curr
            other (pop q)]
        (cond (g/game-over? curr-state)
              curr-path

              (seen curr-state)
              (recur other seen)

              :else
              (recur (apply conj other (for [dir [:left :right :up :down]]
                                         (let [ns (m/move-until-blocked dir curr-state)
                                               np (conj curr-path dir)]
                                           [ns np])))
                     (conj seen curr-state)))))))
