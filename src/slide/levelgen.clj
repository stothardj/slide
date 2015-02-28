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

(defn move-until-blocked [dir s]
  (let [ts (m/get-transitions dir s)
        ns (m/apply-transitions dir ts)]
    (if (m/done-transitioning? ts)
      s
      #(move-until-blocked dir ns))))

(defn simulate-movement [path state]
  (loop [pseq (seq path)
         s state]
    (if-not pseq (:boxes s)
            (let [pnext (seq (rest pseq))
                  dir (first pseq)
                  ns (trampoline move-until-blocked dir s)]
              (recur pnext ns)))))

(defn gen-level [nboxes nrows ncols]
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
        goals (simulate-movement (rand-path 30) start)]
    (assoc start :goals goals)))

(defn new-level []
  (let [lvl (apply gen-level (first @difficulty))]
    (swap! difficulty rest)
    lvl))

