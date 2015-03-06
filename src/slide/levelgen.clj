(ns slide.levelgen
  (:require [clojure.set :as s]
            [slide.draw :as d]
            [slide.move :as m]))

;; Parameters to gen-level.
;; Numbers of rows and cols should be adjusted to avoid distoring on different screen sizes.
;; nboxes, nrows, ncols
(def setups (concat
             [[1 10 6]
              [2 10 6]
              [2 15 9]
              [3 15 9]
              [3 20 12]]
             (repeat [4 20 12])))

(def difficulty (atom setups))

(def wall-density 0.2)

(defn gen-pos [nrows ncols]
  [(rand-int nrows) (rand-int ncols)])

(defn gen-n-pos [n nrows ncols]
  (loop [r #{}]
    (if (= (count r) n) r
        (recur (conj r (gen-pos nrows ncols))))))

(defn rand-path [len]
  (let [f (fn [dirs] (repeatedly #(rand-nth dirs)))
        inf-path (interleave (f [:left :right]) (f [:up :down]))
        inf-path-start (if (zero? (rand-int 2)) inf-path (rest inf-path))]
    (take len inf-path-start)))

(defn try-gen-level [nboxes nrows ncols]
  (let [ps (gen-n-pos nboxes nrows ncols)
        cs (take nboxes (repeatedly #(rand-nth (keys d/named-color))))
        bs (map (partial assoc {} :color) cs)
        boxes (apply assoc {} (interleave ps bs))
        ws (s/difference (gen-n-pos (int (* wall-density (* nrows ncols))) nrows ncols) ps)
        start {:boxes boxes
               :walls ws
               :goals {}
               :bounds {:nrows nrows :ncols ncols}}
        ;; TODO: Goals do not need to be at the end of the movement or require same number of moves
        goals (:boxes (m/simulate-movement (rand-path 30) start))]
    (assoc start :goals goals)))

(defn undesirable-level? [s]
  (let [{:keys [goals boxes]} s
        same-color? (fn [[p g]] (= (:color g) (:color (boxes p))))]
    (some same-color? goals)))

(defn gen-level [nboxes nrows ncols]
  (first (remove undesirable-level? (repeatedly #(try-gen-level nboxes nrows ncols)))))

(defn new-level []
  (let [lvl (apply gen-level (first @difficulty))]
    (swap! difficulty rest)
    lvl))

