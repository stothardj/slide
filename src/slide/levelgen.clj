(ns slide.levelgen
  (:require [clojure.set :as s]
            [slide.draw :as d]
            [slide.move :as m]
            [slide.solve :as slv]))

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

(def levels (atom nil))

(def wall-density 0.2)

(defn gen-pos [nrows ncols]
  [(rand-int nrows) (rand-int ncols)])

(defn gen-n-pos [nrows ncols n]
  (loop [r #{}]
    (if (= (count r) n) r
        (recur (conj r (gen-pos nrows ncols))))))

(defn rand-path [len]
  (let [f (fn [dirs] (repeatedly #(rand-nth dirs)))
        inf-path (interleave (f [:left :right]) (f [:up :down]))
        inf-path-start (if (zero? (rand-int 2)) inf-path (rest inf-path))]
    (take len inf-path-start)))

(defn zip-to-map [ks vs]
  (apply assoc {} (interleave ks vs)))

(defn try-gen-level [nboxes nrows ncols]
  (let [gen (partial gen-n-pos nrows ncols)
        ps (gen nboxes)
        cs (take nboxes (repeatedly #(rand-nth (keys d/named-color))))
        bs (map (partial assoc {} :color) cs)
        boxes (zip-to-map ps bs)
        ws (s/difference (gen (int (* wall-density (* nrows ncols)))) ps)
        walls (zip-to-map ws (repeat {:type :normal}))
        start {:boxes boxes
               :walls walls
               :goals {}
               :bounds {:nrows nrows :ncols ncols}}
        ;; TODO: Goals do not need to be at the end of the movement or require same number of moves
        goals (:boxes (m/simulate-movement (rand-path 30) start))]
    (assoc start :goals goals)))

(defn undesirable-level? [s]
  (let [{:keys [goals boxes]} s
        same-color? (fn [[p g]] (= (:color g) (:color (boxes p))))]
    (some same-color? goals)))

(defn gen-level- [nboxes nrows ncols]
  (let [lvl (try-gen-level nboxes nrows ncols)
        retry #(gen-level- nboxes nrows ncols)]
    (if (undesirable-level? lvl) retry
        (let [solution (slv/solve lvl)]
          (if solution [lvl solution] retry)))))

(defn gen-level [nboxes nrows ncols]
  (trampoline gen-level- nboxes nrows ncols))

(defn gen-all-levels [difficulty-seq]
  (cons (apply gen-level (first difficulty-seq))
        (lazy-seq (gen-all-levels (rest difficulty-seq)))))

(defn new-level []
  (let [lvl (first @levels)]
    (swap! levels rest)
    lvl))

(defn init-state []
  (reset! levels (seque 2 (gen-all-levels setups))))
