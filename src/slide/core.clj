(ns slide.core
  (:require [quil.core :as q]
            [slide.move :as m]
            [slide.draw :as d]
            [slide.levelgen :as lvl]
            [slide.game :as g]))

(def state (atom nil))
(def state-history (atom nil))
(def direction (atom nil))
(def draw-num (atom 0))
(def event-queue (atom nil))

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

(def key-to-event
  {:u :undo
   :r :restart
   :q :quit})

(defn set-direction [dir]
  (if (compare-and-set! direction nil dir)
    (swap! state (partial m/get-transitions dir))))

(defn enqueue-event [ev]
  (swap! event-queue #(conj % ev)))

(defn key-pressed []
  (when-let [dir (key-to-direction (q/key-as-keyword))]
    (set-direction dir))
  (when-let [ev (key-to-event (q/key-as-keyword))]
    (enqueue-event ev)))

(defn push-history [s]
  (swap! state-history #(conj % s)))

(defn pop-history []
  (let [hist (seq @state-history)
        nh (rest hist)
        ns (first nh)]
    (when ns
      (reset! state ns)
      (reset! state-history nh))))

(defn start-level []
  (reset! state (lvl/new-level))
  (reset! state-history nil)
  (push-history @state)
  (reset! direction nil))

(defn setup []
  (d/load-images)
  (start-level)
  (q/smooth)
  (q/frame-rate 60)
  (q/no-stroke)
  (q/background 0))

(defmulti handle-event identity)
(defmethod handle-event :undo [ev]
  (reset! direction nil)
  (pop-history))
(defmethod handle-event :restart [ev]
  (reset! direction nil)
  (let [start (last @state-history)]
    (reset! state start)
    (reset! state-history nil)
    (push-history start)))
(defmethod handle-event :quit [ev]
  (q/exit))

(defn handle-events []
  (when-let [evs (seq @event-queue)]
    (doseq [ev evs]
      (handle-event ev))
    (reset! event-queue nil)))

(defn draw []
  (handle-events)
  (when-let [dir @direction]
    (swap! draw-num #(mod (inc %) d/draws-per-tick))
    (when (zero? @draw-num)
      (let [ss @state
            d (m/done-transitioning? ss)
            ns (m/apply-transitions dir ss)
            ts (if d ns (m/get-transitions dir ns))]
        (when (and (compare-and-set! state ss ts) d)
          (reset! direction nil)
          (push-history ts)))
      (when (g/game-over? @state) (start-level))))

  (d/draw-game @state @direction @draw-num))

(defn -main [& args]
  (q/sketch
   :title "Ice maze"
   :setup setup
   :draw draw
   :size [(:width canvas) (:height canvas)]
   :key-pressed key-pressed))
