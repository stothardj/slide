(ns slide.draw
  (:require [quil.core :as q]))

(def named-color {:red [255 0 0]
                  :green [0 255 0]
                  :blue [0 0 255]})

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

(defn draw-goal [bounds p]
  (let [[r c] p
        rp (row-pos bounds r)
        cp (col-pos bounds c)
        width (square-width bounds)
        height (square-height bounds)]
    (q/triangle cp rp cp (+ rp height) (+ cp width) (+ rp (/ height 2)))))

(defn draw-boxes [boxes bounds]
  (doseq [box boxes]
    (let [[p b] box]
      (apply q/fill (named-color (:color b)))
      (draw-rect bounds p))))

