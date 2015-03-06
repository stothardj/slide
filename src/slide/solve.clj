(ns slide.solve
  (:require [slide.move :as m]
            [slide.game :as g]))

;; Iterative DFS
(defn paths-of-len- [len paths n]
  (if (= len n) paths
      (recur len (for [p paths
                       dir [:left :right :up :down]] (conj p dir)) (inc n))))

(defn paths-of-len [len]
  (paths-of-len- len [[]] 0))

(defn solve-dfs [s]
  (->> (for [len (range)
            path (paths-of-len len)]
         [(m/simulate-movement path s) path])
       (filter (comp g/game-over? first))
       first
       second))

;; BFS

(defn solve-bfs [s]
  (loop [q (into clojure.lang.PersistentQueue/EMPTY [[s []]])]
    (let [curr (peek q)
          [curr-state curr-path] curr
          other (pop q)]
      (if (g/game-over? curr-state) curr-path
          (recur (apply conj other (for [dir [:left :right :up :down]]
                                     (let [ns (m/move-until-blocked dir curr-state)
                                           np (conj curr-path dir)]
                                       [ns np]))))))))
