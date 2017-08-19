(ns warren.core
  (:require [reagent.core :as reagent :refer [atom]] 
            [clojure.string :as str]))

(enable-console-print!) 

(defonce board-size [30 20])
(defonce initial-position [2 2])

(defn blank-board [x y]
  (vec (repeat x (vec (repeat y "0")))))

(defonce state (atom {:text "Welcome to Warren"
                      :board (apply blank-board board-size)
                      :x (first initial-position)
                      :y (second initial-position)}))

(defn add-to-cell [char x y]
  (swap! state update-in [:board x y] 
         #(str char get-in state [:board x y])))

(defn create-walls []
  (for [x (range (first  board-size))
        y (range (second  board-size))]
    (do  
      (if (== x 0) (add-to-cell "n" x y))
      (if (== y 0) (add-to-cell "w" x y))
      (if (== x (first board-size)) (add-to-cell "s" x y))
      (if (== y (second board-size)) (add-to-cell "e" x y)))))

(defn can-move? [x y direction]
  (not (str/includes? (get-in @state [:board x y]) direction)))

(defn move-character! [direction]
  (let [x (:x @state)
        y (:y @state)]
    (if (can-move? x y direction)
        (case direction
          "n" (swap! state assoc :y (dec y))
          "s" (swap! state assoc :y (inc y))
          "e" (swap! state assoc :x (inc x))
          "w" (swap! state assoc :x (dec x))))))

(defn handle-keys! [event]
  (let [key (.-keyCode event)]
    (case key
          32 (println "spacebar")                            ; spacebar
          13 (println "enter")                               ; enter
          37 (move-character! "w")
          38 (move-character! "n")
          39 (move-character! "e")
          40 (move-character! "s")
          (println key))))

(defn circle [x y]
  [:circle
   {:r 0.1
    :stroke "blue"
    :stroke-width 0.5
    :fill "none"
    :cx (+ 0.5 x)
    :cy (+ 0.5 y)}])

(defn border-top [x y]
  [:line {:stroke "black"
          :stroke-width 0.1
          :x1 x
          :x2 (+ x 1) 
          :y1 y
          :y2 y}])

(defn border-bottom [x y]
  [:line {:stroke "black"
          :stroke-width 0.1
          :x1 (+ x 1) 
          :x2 (+ x 1) 
          :y1 y
          :y2 y}])

(defn border-right [x y]
  [:line {:stroke "black"
          :stroke-width 0.1
          :x1 x
          :x2 x 
          :y1 y
          :y2 (+ y 1)}])

(defn border-left [x y]
  [:line {:stroke "black"
          :stroke-width 0.1
          :x1 (+  x 1) 
          :x2 (+  x 1) 
          :y1 y
          :y2 (+  y 1)}])

(defn box [x y]
  [:rect
   {:width 1
    :height 1
    :fill "grey"
    :x x
    :y y}])

(defn stats []
  [:div 
   [:h1 (:text @state)]
   [:h2 "blue"]
   [:h1 (:x @state) (:y @state)]])

(defn tile [x y]
  [:g
   (box x y)
   (for [letter (seq (get-in @state [:board x y]))]
     (case letter
         "n" (border-top x y)
         "s" (border-bottom x y)
         "w" (border-left x y)
         "e" (border-right x y)
         nil))
   (if 
       (and 
        (= x (:x @state))
        (= y (:y @state)))
     (circle x y))])

(defn warren []
  [:center
   [:h1 (:text @state)]
   (let [x (:x @state)
        y (:y @state)]
     [:h2 "X:" x " Y:" y " Tile:" (get-in @state [:board x y])])   
   (into
    [:svg
     {:view-box (str "0 0 "  (first board-size) " " (second board-size))
      :width (* 20 (first board-size))
      :height (* 20 (second board-size))
      :style {:border "1px solid black"}}] 
    (for [i (range (first  board-size))
          j (range (second  board-size))]
      (tile i j)))])

 
(defn on-js-reload []
  (println "Reloading world.")
  ;(reset! state)
  (reagent/render-component [warren]
                            (. js/document (getElementById "app"))))

(defn init []
  (on-js-reload)
  (create-walls)
  (.addEventListener js/document "keydown" handle-keys!))

(defonce start
  (init))
