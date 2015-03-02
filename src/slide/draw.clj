(ns slide.draw
  (:require [quil.core :as q]))

(def images (atom nil))

(defn load-images []
  (reset! images {:boxes {:red (q/load-image "penguin-red.png")
                          :blue (q/load-image "penguin-blue.png")
                          :green (q/load-image  "penguin-green.png")}
                  :goals {:red (q/load-image "fish-red.png")
                          :blue (q/load-image "fish-blue.png")
                          :green (q/load-image "fish-green.png")}
                  :walls (q/load-image "iceblock.png")}))

(def draws-per-tick 3)

(def named-color {:red [255 0 0]
                  :green [0 255 0]
                  :blue [0 0 255]})

(def xy-dir {:left [-1 0]
             :right [1 0]
             :up [0 -1]
             :down [0 1]})

(defn square-width [bounds]
  (/ (q/width) (:ncols bounds)))

(defn square-height [bounds]
  (/ (q/height) (:nrows bounds)))

(defn row-pos [bounds r]
  (* r (square-height bounds)))

(defn col-pos [bounds c]
  (* c (square-width bounds)))

(defn draw-rect [bounds p]
  (let [[r c] p
        rp (row-pos bounds r)
        cp (col-pos bounds c)
        width (square-width bounds)
        height (square-height bounds)]
    (q/rect cp rp width height)))

(defn update-vals [m f] (into {} (for [[k v] m] [k (f v)])))

(defn draw-boxes [boxes bounds]
  (let [box-imgs (:boxes @images)
        sw (square-width bounds)
        sh (square-height bounds)]
    (update-vals box-imgs #(q/resize % sw sh))
    (doseq [box boxes]
      (let [[p b] box
            [r c] p]
        (q/image (box-imgs (:color b)) (col-pos bounds c) (row-pos bounds r))))))

(defn draw-background []
  (q/fill 40 130 160)
  (q/rect 0 0 (q/width) (q/height)))

(defn draw-walls [s]
  (let [walls (:walls s)
        bounds (:bounds s)
        wall-img (:walls @images)
        sw (square-width bounds)
        sh (square-height bounds)]
    (q/resize wall-img sw sh)
    (doseq [[r c] walls]
      (q/image wall-img (col-pos bounds c) (row-pos bounds r)))))

(defn moving-box? [box]
  (let [[p b] box] (some #{:move} (:transition b))))

(defn draw-all-boxes [s dir dn]
  (if dir
    (do
      (q/push-matrix)
      (let [adj-amt (/ dn draws-per-tick)
            adj-vec (map (partial * adj-amt) (xy-dir dir))
            bounds (:bounds s)
            v (map * adj-vec [(square-width bounds) (square-height bounds)])]
        (q/translate v))
      (draw-boxes (filter moving-box? (:boxes s)) (:bounds s))
      (q/pop-matrix)
      (draw-boxes (remove moving-box? (:boxes s)) (:bounds s)))
    (draw-boxes (:boxes s) (:bounds s))))

(defn draw-goals [s]
  (let [goal-imgs (:goals @images)
        bounds (:bounds s)
        sw (square-width bounds)
        sh (square-height bounds)]
    (update-vals goal-imgs #(q/resize % sw sh))
    (doseq [goal (:goals s)]
      (let [[p g] goal
            [r c] p]
        (q/image (goal-imgs (:color g)) (col-pos bounds c) (row-pos bounds r))))))

(defn draw-game [s dir dn]
  (draw-background)
  (draw-walls s)
  (draw-all-boxes s dir dn)
  (draw-goals s))
