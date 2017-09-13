(ns warren.core
  (:require [reagent.core :as reagent :refer [atom]] 
            [clojure.string :as str]))

(enable-console-print!) 

(declare init)

(defonce board-size [30 20])
(defonce initial-position [2 2])
(defonce number-of-rabbits 10)
(defonce number-of-carrots 10)

(defn blank-board [x y]
  (vec (repeat x (vec (repeat y #{:n :s :e :w :u})))))

(defn random-location []
  [(int (rand (first board-size)))
   (int (rand (second board-size)))])

(defonce state (atom {:text "Welcome to Warren"
                      :board (apply blank-board board-size)
                      :x (first initial-position)
                      :y (second initial-position)
                      :digging? false
                      :mouse {:name "Mouscowitz"
                              :position initial-position
                              :health 5
                              :attribs {:str 4 
                                        :int 15 
                                        :wis 9 
                                        :dex 16 
                                        :con 6}
                              :carrots 1
                              :rabbits 0}}))

(defn cell [[x y]]
  (get-in @state [:board x y]))

(defn cell-contains? [key pos]
  (contains? (cell pos) key))

(defn cell-add! [key [x y]]
  (swap! state assoc-in [:board x y] 
         (conj (cell [x y]) key)))

(defn cell-remove! [key [x y]]
  (swap! state assoc-in [:board x y]
         (disj (cell [x y]) key)))

(defn next-cell [dir [x y]]
  (case dir
    :s [x (+ y 1)]
    :n [x (- y 1)]
    :e [(+ x 1) y]
    :w [(- x 1) y]))

(defn opposite-dir [dir]
  (case dir
    :n :s
    :s :n
    :w :e
    :e :w))

(defn remove-wall! [dir [x y]]
  (cell-remove! dir [x y])
  (cell-remove! :u [x y])
  (let [[nx ny] (next-cell dir [x y])]
    (cell-remove! (opposite-dir dir) [nx ny])
    (cell-remove! :u [nx ny])))
 
(def directions [:n :s :e :w])

(defn carve-maze-from [x y]
  "Carve out walls for maze using recursive backtracking algorithm"
  (doall 
   (for [direction (clojure.core/shuffle directions)
         :let [[new_x new_y] (next-cell direction [x y])]
         :when (cell-contains? :u [new_x new_y])]
     (do (remove-wall! direction [x y])
         (carve-maze-from new_x new_y)))))

(defn add-carrots-to-map! [n]
  (take n (repeatedly (cell-add! :c (random-location)))))

(defn add-ferret-to-map! []
  (cell-add! :f (random-location)))

(defn add-rabbits-to-map! [n]
  (take n (repeatedly (cell-add! :r (random-location)))))

(defn found-carrot! [x y]
  (cell-remove! :c [x y])
  (js/alert "You found a carrot!")
  (swap! state update-in [:mouse :carrots] inc))

(defn found-rabbit! [x y]
  (cell-remove! :r [x y])
  (js/alert "You found a trapped rabbit. Save it!")
  (swap! state update-in [:mouse :rabbits] inc))

(defn found-ferret! []
  (js/alert  "The ferret caught you.  You die...")
  (init))

(defn eat-carrot! []
  (if (> (get-in @state [:mouse :carrots]) 0)
    (do
      (js/alert "You eat a carrot. Yumm.")
      (swap! state update-in [:mouse :carrots] dec)
      (swap! state update-in [:mouse :health] inc))
    (js/alert "You cannot eat carrots you don't have!")))

(defn check-cell [x y]
  (if (cell-contains? :c [x y]) (found-carrot! x y))
  (if (cell-contains? :r [x y]) (found-rabbit! x y))
  (if (cell-contains? :f [x y]) (found-ferret!)))

(defn move-character! [direction]
  (let [x (:x @state)
        y (:y @state)]
    (if (not (cell-contains? direction [x y]))
      (let [[new_x new_y] (next-cell direction [x y])]
        (cell-add! :v [new_x new_y])
        (swap! state assoc :x new_x)
        (swap! state assoc :y new_y)
        (check-cell new_x new_y)))))

(defn dig-mode! []
  (if (> (get-in @state [:mouse :rabbits]) 0)
    (if (js/confirm "Start Digging?")
      (swap! state assoc :digging? true))
    (js/alert "You can't dig without a rabbit!")))


(defn dig! [dir]
  (let [x (:x @state)
        y (:y @state)]
    (remove-wall! dir [x y]))
  (swap! state assoc :digging? false)
  (swap! state update-in [:mouse :rabbits] dec)
  (add-rabbits-to-map! 1)
  (js/alert "The rabbit dug through the wall.  And ran away!"))

(defn handle-keys! [event]
  (let [key (.-keyCode event)]
    (if (:digging? @state)
      (case key
        37 (dig! :w)
        38 (dig! :n)
        39 (dig! :e)
        40 (dig! :s)))
    (case key
        32 (println "spacebar")       
        13 (println "enter")  
        69 (eat-carrot!)
        68 (dig-mode!)
        37 (move-character! :w)
        38 (move-character! :n)
        39 (move-character! :e)
        40 (move-character! :s)
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
            :x1 (+ x 1)
            :x2 (+ x 1) 
            :y1 y
            :y2 (+ y 1)}])

(defn border-left [x y]
  [:line {:stroke "black"
          :stroke-width 0.1
          :x1 x
          :x2 x 
          :y1 y
          :y2 (+  y 1)}])

(defn box [color x y]
  [:rect
   {:width 1
    :height 1
    :fill color
    :x x
    :y y}])

(defn tile [x y]
  [:g
   (box "grey" x y)
   (if (cell-contains? :v [x y]) (box "darkgrey" x y)) 
   (if (cell-contains? :n [x y]) (border-top x y))
   (if (cell-contains? :s [x y]) (border-bottom x y))
   (if (cell-contains? :e [x y]) (border-right x y))
   (if (cell-contains? :w [x y]) (border-left x y))
   (if (and (= x (:x @state))(= y (:y @state)))(circle x y))])

(defn warren []
  [:center
   [:h1 (:text @state)]
   [:h2 "Carrots: " (get-in @state [:mouse :carrots]) 
    "   Rabbits: " (get-in @state [:mouse :rabbits]) "/" number-of-rabbits]
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
  (reagent/render-component [warren]
                            (. js/document (getElementById "app"))))

(defn init []
  (on-js-reload)
  (apply carve-maze-from initial-position)
  (cell-add! :v initial-position)
  (add-carrots-to-map! number-of-carrots)
  (add-ferret-to-map!)
  (add-rabbits-to-map! number-of-rabbits)
  (.addEventListener js/document "keydown" handle-keys!))

(defonce start
  (init))
