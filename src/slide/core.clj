(ns slide.core
  (:require [quil.core :as q]
            [slide.move :as m]
            [slide.draw :as d]
            [slide.levelgen :as lvl]))

(def state (atom nil))
(def direction (atom nil))
(def draw-num (atom 0))

(def canvas {:width 300
             :height 500})

(def key-to-direction
  {:left :left
   :right :right
   :up :up
   :down :down
   :a :left
   :d :right
   :w :up
   :s :down})

(defn key-pressed []
  (when-let [dir (key-to-direction (q/key-as-keyword))]
    (if (compare-and-set! direction nil dir)
      (swap! state (partial m/get-transitions dir)))))

(defn start-level []
  ;; (reset! state (lvl/gen-level 4 20 12))
  (reset! state (lvl/new-level))
  (reset! direction nil))

(defn setup []
  (d/load-images)
  (start-level)
  (q/smooth)
  (q/frame-rate 60)
  (q/no-stroke)
  (q/background 0))

(defn game-over? [s]
  (empty? (:goals s)))

(defn draw []
  (when-let [dir @direction]
    (swap! draw-num #(mod (inc %) d/draws-per-tick))
    (when (zero? @draw-num)
      (let [ss @state
            d (m/done-transitioning? ss)
            ns (m/apply-transitions dir ss)
            ts (if d ns (m/get-transitions dir ns))]
        (when (and (compare-and-set! state ss ts) d) (reset! direction nil)))
      (when (game-over? @state) (start-level))))

  (d/draw-game @state @direction @draw-num))

(defn -main [& args]
  (q/sketch
   :title "Ice maze"
   :setup setup
   :draw draw
   :size [(:width canvas) (:height canvas)]
   :key-pressed key-pressed))
