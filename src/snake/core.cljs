(ns snake.core
  (:require [cljs.core.async :refer [<! timeout go]]))

;; FUNCTIONAL
(def board-size 100)
(def square-size 8)
(def initial-apple [(rand-int board-size) (rand-int board-size)])
(def initial-snake {:body '([50 50] [49 49] [48 48])
                    :direction [-1 0]
                    :growing false})

(def game-running (atom false))
(def apple (atom initial-apple))

(def snake
  (atom initial-snake))

(defn move [snake]
  (let [body (snake :body)
        head (first body)
        direction (snake :direction)
        growing (snake :growing)]
    (if growing
      (-> (assoc snake :body (conj body (map + head direction)))
          (assoc :growing false))
      (assoc snake :body (butlast (conj body (map + head direction)))))))

(defn change-direction [snake direction]
  (case direction
    :up (assoc snake :direction [0 -1])
    :down (assoc snake :direction [0 1])
    :left (assoc snake :direction [-1 0])
    :right (assoc snake :direction [1 0])))

(defn has-colided-itself? [{:keys [body]}]
  ((into #{} (rest body)) (first body)))

(defn has-hit-wall? [{[[x y] & _] :body}]
  (or (< x 0)
      (< y 0)
      (> x board-size)
      (> y board-size)))

(defn has-hit-apple? [{[[x y] & _] :body} [ax ay]]
  (and (= x ax) (= y ay)))

;; STATEFUL
(defn draw-rect [ctx rect]
  (let [[x y] rect]
    (.fillRect ctx (* square-size x) (* square-size y) square-size square-size)))

(defn clear-rect [ctx rect]
  (let [[x y] rect]
    (.clearRect ctx (* x square-size) (* square-size y) square-size square-size)))

(defn draw-snake [ctx snake]
  (draw-rect ctx (first (snake :body)))
  (when-not (:growing snake)
    (clear-rect ctx (last (snake :body)))))

(defn action! [ctx snake apple]
  (cond (has-hit-wall? @snake)
        (swap! game-running not)
        (has-colided-itself? @snake)
        (swap! game-running not)
        (has-hit-apple? @snake @apple)
        (do (swap! snake assoc :growing true)
            (clear-rect ctx @apple)
            (reset! apple [(rand-int board-size) (rand-int board-size)]))))

(def board (.getElementById js/document "board"))
(def ctx (.getContext board "2d"))

(defn game-loop []
  (go
    (loop []
     (when @game-running
       (<! (timeout 50))
       (draw-rect ctx @apple)
       (swap! snake move)
       (action! ctx snake apple)
       (draw-snake ctx @snake)
       (recur)))))

(defn reset-game! []
  (reset! snake initial-snake)
  (reset! apple initial-apple))

(defn check-key [e]
  (case (.-code e)
    "ArrowUp"
    (swap! snake change-direction :up)
    "ArrowDown"
    (swap! snake change-direction :down)
    "ArrowLeft"
    (swap! snake change-direction :left)
    "ArrowRight"
    (swap! snake change-direction :right)
    "Space"
    (if @game-running
      (swap! game-running not)
      (do (reset-game!)
          (swap! game-running not)
          (.clearRect ctx 0 0 (* board-size square-size) (* board-size square-size))
          (game-loop)))

    nil))

(defn ^:export game []
  (js/document.addEventListener "keydown" check-key)
  (game-loop))
