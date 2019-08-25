(ns quantum-chess.visualisation
  (:require
    [quantum-chess.validator]
    [quantum-chess.constants :as constants]
    [quantum-chess.game :as game]
    ))

(defn click
  [game-state-atom display-state-atom x y]
  (let [{:keys [?clicked] :as display-state} @display-state-atom]
    (println "Click:" x y)
    (if ?clicked
      (let [from ?clicked
            to {:x x :y y}]
        (swap! display-state-atom dissoc :?clicked)
        (swap! game-state-atom game/make-move from to)
        (swap! display-state-atom assoc :turn (game/get-turn-num @game-state-atom))
        )
      (swap! display-state-atom assoc :?clicked {:x x :y y}))))
(defn display-square
  [game-state click-fn turn-num x y]
  ^{:key {:x x :y y}}
  [:td
   {:onClick (partial click-fn x y)}
   (if-let [piece-id (get-in game-state [:turns turn-num :derived/coords {:x x :y y}])]
     [:div
      (str \| x \space y \space piece-id \space (get-in game-state [:pieces piece-id :color]) \|)
      [:br]
      (get-in game-state [:derived/possibles piece-id])
      ]
     (str \| x \space y " - -|"))])
(defn display-board
  [game-state-atom display-state-atom]
   (let [{:keys [turn] :as display-state} @display-state-atom
         game-state (assoc @game-state-atom :derived/coords (-> @game-state-atom :board (nth turn) constants/derived-coords))
         turns (game/get-turn-num game-state)
         click-fn (fn [x y] (click game-state-atom display-state-atom x y))
         ]
     [:table.table.table-hover>tbody
      (doall (for [y (range 0 (-> game-state :board-stats :width))]
               ^{:key y}
               [:tr (doall (for [x (range 0 (-> game-state :board-stats :height))]
                             (display-square
                               game-state
                               (if (= turn turns) click-fn) ; Only allow clicking on current turn
                               (:turn @display-state-atom)
                               x y)))]))]))

(defn display-slider
  [game-state-atom display-state-atom]
  (let [game-state @game-state-atom
        {:keys [turn] :as display-state} @display-state-atom
        turns (-> game-state :turns count dec)
        percentage (if (zero? turns) "100" (-> (/ turn turns) (* 100) (js/parseInt)))]
    [:div
     [:button.btn
      {:on-click (partial swap! display-state-atom update-in [:turn] #(max 0 (dec %)))}
      "<"]
     (str "Turn: " turn "/" turns)
     [:button.btn
      {:on-click (partial swap! display-state-atom update-in [:turn] #(min turns (inc %)))}
      ">"]
     [:span.progress
      [:div.progress-bar {:style {:width (str percentage "%") :aria-valuenow (str percentage) :aria-valuemin "0" :aria-valuemax "100"}}]
      ]
     ]))
