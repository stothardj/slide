(ns slide.move)

(def named-dir {:left [0 -1]
                :right [0 1]
                :up [-1 0]
                :down [1 0]})

(defn move [dir p] (mapv + p (named-dir dir)))

(defn in-bounds? [bounds p]
  (let [[r c] p
        {:keys [nrows ncols]} bounds]
    (and (>= r 0)
         (< r nrows)
         (>= c 0)
         (< c ncols))))

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

              ;; Goal of the correct color has been reached
              (= (:color b) (:color (goals np)))
              (recur ps pset
                     (-> ret
                         (assoc-in [:goals np :transition] [:disappear])
                         (assoc-in [:boxes p :transition] [:move :disappear])))

              ;; Not hitting anything
              :else
              (recur ps pset (assoc-in ret [:boxes p :transition] [:move]))))))))

(defn done-transitioning? [s]
  (every? (fn [[p b]] (some #{:stay} (:transition b))) (:boxes s)))

(defn- apply-box-transition [dir box]
  (let [[p b] box
        nb (dissoc b :transition)
        t (:transition b)]
    (cond
      (some #{:disappear} t) nil
      (some #{:move} t) [(move dir p) nb]
      (some #{:stay} t) [p nb])))

(defn- apply-goal-transition [goal]
  (let [[p g] goal
        ng (dissoc g :transition)
        t (:transition g)]
    (when-not (some #{:disappear} t) [p ng])))

(defn apply-transitions [dir s]
  (-> s
      (update-in [:boxes] #(into {} (map (partial apply-box-transition dir) %)))
      (update-in [:goals] #(into {} (map apply-goal-transition %)))))

(defn- move-until-blocked- [dir s]
  (let [ts (get-transitions dir s)
        ns (apply-transitions dir ts)]
    (if (done-transitioning? ts)
      s
      #(move-until-blocked- dir ns))))

(defn move-until-blocked [dir s]
  (trampoline move-until-blocked- dir s))

(defn simulate-movement [path state]
  (loop [pseq (seq path)
         s state]
    (if-not pseq s
            (let [pnext (seq (rest pseq))
                  dir (first pseq)
                  ns (move-until-blocked dir s)]
              (recur pnext ns)))))
