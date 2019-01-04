(ns consnake.core
  (:gen-class)
  (:require [lanterna.screen :as s]))

(def target-fps (atom 20))
(defn optimal-time [] (double (/ 1000000000 @target-fps)))
(def delta (atom 0))

(def now (atom 0))
(def update-length (atom 0))
(def last-loop-time (atom 0))
(def last-fps (atom 0))
(def fps (atom 0))
(def running (atom true))
(def apples (atom []))
(def game-over (atom false))
(def pause (atom false))
(def restart-game (atom false))
(def best-score (atom 0))

(def snake (atom []))


(defn get-time []
  (java.lang.System/nanoTime))


(def screen-size (ref [0 0]))

(def scr (atom nil))

(defn handle-resize [cols rows]
  (dosync  (do
    (ref-set screen-size [cols rows])
    (s/clear @scr))))

(def x (atom 110))
(def y (atom 20))
(def dir-x (atom -1))
(def dir-y (atom 0))
(def last-x (atom 0))
(def last-y (atom 0))
(def size (atom 3))

(defn get-cols []
  (max (nth (s/get-size @scr) 0) 0))


(defn get-rows []
  (max  (nth (s/get-size @scr) 1) 0))

(defn upper-bar []
  (reduce str (repeat (get-cols) "="))
  (reduce str (repeat (get-cols) "=")))

(defn get-cell [index]
  (nth @snake index))

(defn get-apple-cell [index]
  (nth @apples index))

(defn drop-nth [n coll]
   (keep-indexed #(if (not= %1 n) %2) coll))

(defn set-cell [index v]
  (reset! snake (update-in @snake [index] :assoc v)))

(defn collision []
  (when (or (>= (:x (get-cell 0)) (get-cols))
            (>= (:y (get-cell 0)) (get-rows))
            (< (:x (get-cell 0)) 0)
            (< (:y (get-cell 0)) 0))
    (reset! game-over true))
   (loop [i 0]
     (when (< i (count @apples))
       (when (and (=
                    (:x (get-cell 0))
                    (:x (get-apple-cell i)))
                  (= (:y (get-cell 0))
                     (:y (get-apple-cell i))))
         (reset! apples (drop-nth i @apples))
         (swap! apples conj {:x (int (rand (get-cols))) :y (int (+ (rand (get-rows)) 3))})
         (reset! apples (distinct @apples))
         (when (= 0 (mod @size 10 ))
          (reset! target-fps (inc @target-fps)))
         (reset! size (+ @size 1)))

       (recur (inc i)))))

(defn restart []
  (reset! x (- (get-cols) 5))
  (reset! y (int (+ (rand (get-rows)) 3)))
  (reset! dir-x -1)
  (reset! dir-y 0)
  (reset! size 3)
  (reset! snake [])
  (reset! apples [])
  (reset! target-fps 20)
;( int (* (get-cols) (get-rows) 0.01))
  (loop [i 0]
    (when (< i ( int (* (get-cols) (get-rows) 0.03)))
      ;;prevent same apples distinct
      (swap! apples conj {:x (int (rand (get-cols))) :y (int (+ (rand (get-rows)) 3))})
      (recur (inc i))))
  (reset! apples (distinct @apples))

  (loop [i 0]
    (when (< i @size)
      (swap! snake conj {:x (+ @x i) :y @y})
      (recur (inc i))))
  )
(defn draw []

   (s/put-string @scr 0 0 (str "size: " (get-cols) "x" (get-rows)))
   (s/put-string @scr 0 1 (str "score: " @size))
   (s/put-string @scr 0 2 (str "fps: " @target-fps))

   (when @pause
    (s/put-string @scr (int (/ (get-cols) 4)) 0 "PAUSED" {:fg :yellow}))
   (when @game-over
    (s/put-string @scr (int (/ (get-cols) 2)) 0 "GAME OVER!" {:fg :yellow})
    (reset! best-score (max @size @best-score)))
   (loop [i 0]
     (when (< i (count @apples))
        (s/put-string @scr (:x (get-apple-cell i)) (:y (get-apple-cell i)) "@" {:fg :red})
       (recur (inc i))))

   (loop [i 0]
     (when (< i @size)
       (if (even? i)
        (s/put-string @scr (:x (get-cell i)) (:y (get-cell i)) (if (= i 0) "8" " ") {:bg :green :fg :black})
        (s/put-string @scr (:x (get-cell i)) (:y (get-cell i)) (if (= i 0) "8" " ") {:bg :green :fg :black}))
       (when (and (=
                    (:x (get-cell 0))
                    (:x (get-cell i)))
                  (= (:y (get-cell 0))
                     (:y (get-cell i))) (> i 0))
        (reset! game-over true))
       (recur (inc i)))))

(defn update-game []
    (reset! x (+ @x (* @dir-x 1)))
    (reset! y (+ @y (* @dir-y 1)))
    (loop [i  @size ]
      (when (> i 0)
        (set-cell i (get-cell (- i 1)))
        (recur (dec i))))
    (set-cell 0 {:x @x :y @y}))

(defn get-input []
  (while @running
    ;(println "input seeking")
   (let [user-input (s/get-key @scr)]
     (cond
       (or (= \q user-input) (= :escape user-input)) (reset! running false)
       (and (or @game-over @pause ) (= \r user-input)) (do (reset! game-over false) (reset! restart-game true))
       (or (= \p user-input) (= :backspace user-input)) (reset! pause (not @pause))
       (or (= \a user-input) (= :left user-input)) (do (reset! dir-x -1) (reset! dir-y 0))
       (or (= \d user-input) (= :right user-input)) (do (reset! dir-x 1) (reset! dir-y 0))
       (or (= \w user-input) (= :up user-input)) (do (reset! dir-y -1) (reset! dir-x 0))
       (or (= \s user-input) (= :down user-input)) (do (reset! dir-y 1) (reset! dir-x 0))))
   (Thread/sleep 10)))

(defn game-loop []
  (reset! last-loop-time (get-time))
  (while @running
    (s/clear @scr)
    (reset! now (get-time))
    (reset! update-length (- @now @last-loop-time))
    (reset! last-loop-time @now)
    (reset! delta (double (/ @update-length (optimal-time))))
    (reset! last-fps (+ @update-length @last-fps))
    (reset! fps (inc @fps))
    (when (>= @last-fps 1000000000)
      (reset! last-fps 0)
      (reset! fps 0))
    (when @restart-game
      (restart)
      (reset! restart-game false))

    (when-not (or @game-over @pause)
      (update-game)
      (collision))
    (draw)

    (s/redraw @scr)

    (Thread/sleep (max 0 (double (/ (+ (optimal-time) (- @last-loop-time (get-time))) 1000000))))))

(defn -main []
  (reset! scr (s/get-screen :text {:resize-listener handle-resize}))
  ;;init snake
  (.start (Thread. (fn [] (get-input))))
  (s/start @scr)
  (restart)
  (game-loop)
  (reset! running false)
  (s/stop @scr)
  (println "Your best score: " @best-score))

