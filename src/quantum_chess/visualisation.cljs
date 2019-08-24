(ns quantum-chess.visualisation
  (:require
    [quantum-chess.validator]
    [quantum-chess.constants :as constants]
    ))

(defn display-square
  [game-state x y]
  [:td
  (if-let [piece-id (get-in game-state [:derived/coords {:x x :y y}])]
    (str \| x \space y \space piece-id \space (get-in game-state [:pieces piece-id :color]) \|)
    (str \| x \space y " - -|")
    )])
(defn display-board
  [game-state]
   [:table.table.table-hover>tbody
    (doall (for [y (range 0 8)]
      [:tr (doall (for [x (range 0 8)] (display-square game-state x y)))]))])
