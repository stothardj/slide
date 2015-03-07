(ns slide.core
  (:require [quil.core :as q]
            [slide.move :as m]
            [slide.draw :as d]
            [slide.levelgen :as lvl]
            [slide.game :as g]
            [slide.solve :as s]))

(def mode (atom :play))
(def state (atom nil))
(def state-history (atom nil))
(def direction (atom nil))
(def draw-num (atom 0))
(def event-queue (atom nil))
(def solution (atom nil))

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
   :g :giveup
   :q :quit})

(defn try-set-direction [dir]
  (if (compare-and-set! direction nil dir)
    (swap! state (partial m/get-transitions dir))))

(defn set-direction [dir]
  (reset! direction dir)
  (swap! state (partial m/get-transitions dir)))

(defn enqueue-event [ev]
  (swap! event-queue #(conj % ev)))

(defn key-pressed []
  (when-let [dir (key-to-direction (q/key-as-keyword))]
    (try-set-direction dir))
  (when-let [ev (key-to-event (q/key-as-keyword))]
    (enqueue-event ev)))

(defn mouse-clicked []
  (when-let [ev (d/get-button (q/mouse-x) (q/mouse-y))]
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
  (reset! mode :play)
  (let [[lvl sol] (lvl/new-level)]
    (reset! state lvl)
    (reset! solution sol))
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

(defn restart-level []
  (reset! direction nil)
  (let [start (last @state-history)]
    (reset! state start)
    (reset! state-history nil)
    (push-history start)))

(defmulti handle-event identity)
(defmethod handle-event :undo [ev]
  (reset! direction nil)
  (pop-history))
(defmethod handle-event :restart [ev]
  (restart-level))
(defmethod handle-event :giveup [ev]
  (restart-level)
  (reset! mode :show)
  (set-direction (first @solution)))
(defmethod handle-event :quit [ev]
  (q/exit))

(defn handle-events []
  (when-let [evs (seq @event-queue)]
    (doseq [ev evs]
      (handle-event ev))
    (reset! event-queue nil)))

(defn tick []
  (swap! draw-num #(mod (inc %) d/draws-per-tick))
  (zero? @draw-num))

(defn handle-direction []
  (when-let [dir @direction]
    (when (tick)
      (let [ss @state
            d (m/done-transitioning? ss)
            ns (m/apply-transitions dir ss)
            ts (if d ns (m/get-transitions dir ns))]
        (when (and (compare-and-set! state ss ts) d)
          (when (= @mode :play)
            (reset! direction nil)
            (push-history ts))
          (when (= @mode :show)
            (swap! solution rest)
            (set-direction (first @solution)))))
      (when (g/game-over? @state) (start-level)))))

(defn draw []
  (when (= :play @mode)
    (handle-events))
  (handle-direction)

  (d/draw-game @state @direction @draw-num))

(defn init-state []
  (reset! mode :play)
  (reset! state nil)
  (reset! state-history nil)
  (reset! direction nil)
  (reset! draw-num 0)
  (reset! event-queue nil)
  (reset! solution nil)
  (lvl/init-state))

(defn -main [& args]
  (init-state)
  (q/sketch
   :title "Picky Penguins"
   :setup setup
   :draw draw
   :size [(:width d/canvas) (:height d/canvas)]
   :key-pressed key-pressed
   :mouse-clicked mouse-clicked))
