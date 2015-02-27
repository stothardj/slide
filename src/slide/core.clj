(ns slide.core
  (:require [quil.core :as q]
            [slide.move :as m]
            [slide.draw :as d]
            [slide.levelgen :as lvl]))

(def xy-dir {:left [-1 0]
             :right [1 0]
             :up [0 -1]
             :down [0 1]})

(def draws-per-tick 2)

(def state (atom (lvl/gen-level 4 20 12)))
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

(defn setup []
  (q/smooth)
  (q/frame-rate 60)
  (q/no-stroke)
  (q/background 0))

(defn moving-box? [box]
  (let [[p b] box] (some #{:move} (:transition b))))

(defn game-over? [s]
  (empty? (:goals s)))

(defn draw []
  (when-let [dir @direction]
    (swap! draw-num #(mod (inc %) draws-per-tick))
    (when (zero? @draw-num)
      (let [ss @state
            d (m/done-transitioning? ss)
            ns (m/apply-transitions dir ss)
            ts (if d ns (m/get-transitions dir ns))]
        (when (and (compare-and-set! state ss ts) d) (reset! direction nil)))
      (when (game-over? @state)
        (reset! state (lvl/gen-level 4 20 12))
        (reset! direction nil))))

  ;; TODO: proper way to clear the canvas?
  (q/fill 0)
  (q/rect 0 0 (q/width) (q/height))

  ;; Draw walls
  (q/fill 150)
  (doseq [w (:walls @state)]
    (d/draw-rect (:bounds @state) w))

  ;; Draw boxes
  (if-let [dir @direction]
    (do
      (q/push-matrix)
      (let [adj-amt (/ @draw-num draws-per-tick)
            adj-vec (map (partial * adj-amt) (xy-dir dir))
            bounds (:bounds @state)
            v (map * adj-vec [(d/square-width bounds) (d/square-height bounds)])]
        (q/translate v))
      (d/draw-boxes (filter moving-box? (:boxes @state)) (:bounds @state))
      (q/pop-matrix)
      (d/draw-boxes (remove moving-box? (:boxes @state)) (:bounds @state)))
    (d/draw-boxes (:boxes @state) (:bounds @state)))

  ;; Draw goals
  (doseq [goal (:goals @state)]
    (let [[p g] goal]
      (apply q/fill (d/named-color (:color g)))
      (d/draw-goal (:bounds @state) p))))

(defn -main [& args]
  (q/sketch
   :title "Ice maze"
   :setup setup
   :draw draw
   :size [(:width canvas) (:height canvas)]
   :key-pressed key-pressed))
