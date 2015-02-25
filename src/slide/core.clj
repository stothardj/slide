(ns slide.core
  (:require [quil.core :as q]))

(def level1 {:boxes {[2 3] {:color :red} [3 3] {:color :blue} [2 6] {:color :green}}
             :walls #{[2 1] [7 3]}
             :goals {[0 3] {:color :red}}
             :bounds {:top 0 :left 0 :bottom 20 :right 12}})

(def draws-per-tick 3)

(def state (atom level1))
(def direction (atom nil))
(def draw-num (atom 0))

(def canvas {:width 300
             :height 500})

(def named-color {:red [255 0 0]
                  :green [0 255 0]
                  :blue [0 0 255]})

(def named-dir {:left [0 -1]
                :right [0 1]
                :up [-1 0]
                :down [1 0]})

(def xy-dir {:left [-1 0]
             :right [1 0]
             :up [0 -1]
             :down [0 1]})

(def key-to-direction
  {:left :left
   :right :right
   :up :up
   :down :down})


(defn move [dir p] (mapv + p (named-dir dir)))

(defn in-bounds? [bounds p]
  (let [[r c] p
        {:keys [top left bottom right]} bounds]
    (and (>= r top)
         (< r bottom)
         (>= c left)
         (< c right))))

(defn get-transitions [dir s]
  (let [{:keys [boxes walls goals bounds]} s]
    (loop [pending (into clojure.lang.PersistentQueue/EMPTY (keys boxes))
           pending-set (into #{} (keys boxes))
           ret s]
      (if (empty? pending) ret
          (let [p (peek pending)
                b (boxes p)
                ps (pop pending)
                pset (disj pending-set p)
                np (move dir p)
                nboxes (:boxes ret)]
            (cond
              ;; Goal of the correct color has been reached
              (= (:color b) (:color (goals np)))
              (recur ps pset
                     (-> ret
                         (assoc-in [:goals np :transition] [:disappear])
                         (assoc-in [:boxes p :transition] [:move :disappear])))

              ;; Hitting a wall
              (contains? walls np)
              (recur ps pset (assoc-in ret [:boxes p :transition] [:stay]))

              ;; Going out of bounds
              (not (in-bounds? bounds np))
              (recur ps pset (assoc-in ret [:boxes p :transition] [:stay]))
              
              ;; Hitting a box which we have yet to determine whether it moves
              (contains? pset np)
              (recur (conj ps p) pending-set ret)
              
              ;; Hitting a box which we have determined is going to stay
              (some #{:stay} (:transition (nboxes np)))
              (recur ps pset (assoc-in ret [:boxes p :transition] [:stay]))

              ;; Not hitting anything
              :else
              (recur ps pset (assoc-in ret [:boxes p :transition] [:move]))))))))

(defn done-transitioning? [s]
  (every? (fn [[p b]] (some #{:stay} (:transition b))) (:boxes s)))

(defn apply-box-transition [dir box]
  (let [[p b] box
        nb (dissoc b :transition)
        t (:transition b)]
    (cond
      (some #{:disappear} t) nil
      (some #{:move} t) [(move dir p) nb]
      (some #{:stay} t) [p nb])))

(defn apply-goal-transition [goal]
  (let [[p g] goal
        ng (dissoc g :transition)
        t (:transition g)]
    (when-not (some #{:disappear} t) [p ng])))

(defn apply-transitions [dir s]
  (-> s
      (update-in [:boxes] #(into {} (map (partial apply-box-transition dir) %)))
      (update-in [:goals] #(into {} (map apply-goal-transition %)))))

(defn square-width [width bounds]
  (/ width (- (:right bounds) (:left bounds))))

(defn square-height [height bounds]
  (/ height (- (:bottom bounds) (:top bounds))))

(defn row-pos [height bounds r]
  (* r (square-height height bounds)))

(defn col-pos [width bounds c]
  (* c (square-width width bounds)))

(defn key-pressed []
  (when-let [dir (key-to-direction (q/key-as-keyword))]
    (if (compare-and-set! direction nil dir)
      (swap! state (partial get-transitions dir)))))

(defn setup []
  (q/smooth)
  (q/frame-rate 60)
  (q/no-stroke)
  (q/background 0))

(defn draw-rect [bounds p]
  (let [[r c] p
        rp (row-pos (:height canvas) bounds r)
        cp (col-pos (:width canvas) bounds c)
        width (square-width (:width canvas) bounds)
        height (square-height (:height canvas) bounds)]
    (q/rect cp rp width height)))

(defn draw-goal [bounds p]
  (let [[r c] p
        rp (row-pos (:height canvas) bounds r)
        cp (col-pos (:width canvas) bounds c)
        width (square-width (:width canvas) bounds)
        height (square-height (:height canvas) bounds)]
    (q/triangle cp rp cp (+ rp height) (+ cp width) (+ rp (/ height 2)))))

(defn draw-boxes [boxes]
  (doseq [box boxes]
    (let [[p b] box]
      (apply q/fill (named-color (:color b)))
      (draw-rect (:bounds @state) p))))

(defn moving-box? [box]
  (let [[p b] box] (some #{:move} (:transition b))))

(defn draw []
  (when-let [dir @direction]
    (swap! draw-num #(mod (inc %) draws-per-tick))
    (when (zero? @draw-num)
      (let [ss @state
            d (done-transitioning? ss)
            ns (apply-transitions dir ss)
            ts (if d ns (get-transitions dir ns))]
        (when (and (compare-and-set! state ss ts) d) (reset! direction nil)))))

  ;; TODO: proper way to clear the canvas?
  (q/fill 0)
  (q/rect 0 0 (:width canvas) (:height canvas))

  ;; Draw walls
  (q/fill 150)
  (doseq [w (:walls @state)]
    (draw-rect (:bounds @state) w))

  ;; Draw boxes
  (if-let [dir @direction]
    (do
      (q/push-matrix)
      (let [adj-amt (/ @draw-num draws-per-tick)
            adj-vec (map (partial * adj-amt) (xy-dir dir))
            bounds (:bounds @state)
            v (map * adj-vec [(square-width (:width canvas) bounds) (square-height (:height canvas) bounds)])]
        (q/translate v))
      (draw-boxes (filter moving-box? (:boxes @state)))
      (q/pop-matrix)
      (draw-boxes (remove moving-box? (:boxes @state))))
    (draw-boxes (:boxes @state)))

  ;; Draw goals
  (doseq [goal (:goals @state)]
    (let [[p g] goal]
      (apply q/fill (named-color (:color g)))
      (draw-goal (:bounds @state) p))))

(q/defsketch example
  :title "Ice maze"
  :setup setup
  :draw draw
  :size [(:width canvas) (:height canvas)]
  :key-pressed key-pressed)
