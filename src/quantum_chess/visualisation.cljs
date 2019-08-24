(ns quantum-chess.visualisation
  (:require
    [quantum-chess.validator]
    [quantum-chess.constants :as constants]
    [quantum-chess.game :as game]
    ))

(defn display-square
  [game-state turn-num x y]
  ^{:key {:x x :y y}}
  [:td
   (if-let [piece-id (get-in game-state [:turns turn-num :derived/coords {:x x :y y}])]
     (str \| x \space y \space piece-id \space (get-in game-state [:pieces piece-id :color]) \|)
     (str \| x \space y " - -|")
     )])
(defn display-board
  [game-state display-state-atom]
   (let [{:keys [turn] :as display-state} @display-state-atom
         game-state (assoc game-state :derived/coords (-> game-state :board (nth turn) constants/derived-coords))
         ]
     [:table.table.table-hover>tbody
      (doall (for [y (range 0 (-> game-state :board-stats :width))]
               ^{:key y}
               [:tr (doall (for [x (range 0 (-> game-state :board-stats :height))] (display-square game-state (:turn @display-state-atom) x y)))]))]))

(defn display-slider
  [game-state display-state-atom]
  (let [{:keys [turn] :as display-state} @display-state-atom
        turns (-> game-state :board count dec)
        percentage (if (zero? turns) "100" (-> (/ turn turns) (* 100) (js/parseInt)))]
    [:div
     [:button.btn
      {:on-click (partial swap! display-state-atom update-in [:turn] #(max 0 (dec %)))}
      "<"]
     [:span.progress
      [:div.progress-bar {:style {:width (str percentage "%") :aria-valuenow (str percentage) :aria-valuemin "0" :aria-valuemax "100"}}]
      ]
     [:button.btn
      {:on-click (partial swap! display-state-atom update-in [:turn] #(min turns (inc %)))}
      ">"]
     ]))
