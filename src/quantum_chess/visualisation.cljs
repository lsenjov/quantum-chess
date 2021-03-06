(ns quantum-chess.visualisation
  (:require
    [quantum-chess.validator]
    [quantum-chess.constants :as constants]
    [quantum-chess.game :as game]
    [quantum-chess.control]
    ))

(defn click
  [game-state-atom display-state-atom x y]
  (try
    (let [{:keys [?clicked] :as display-state} @display-state-atom]
      (println "Click:" x y)
      (if ?clicked
        (let [from ?clicked
              to {:x x :y y}]
          (swap! display-state-atom dissoc :?clicked)
          (swap! game-state-atom quantum-chess.control/move-baby-move from to)
          (swap! display-state-atom assoc :turn (game/get-turn-num @game-state-atom))
          )
        (swap! display-state-atom assoc :?clicked {:x x :y y})))
    (catch js/Error e (js/alert e))))
(defn display-square
  [game-state click-fn turn-num {:keys [x y] :as coord} selected-coord]
  ^{:key {:x x :y y}}
  [:div
   {:style {:width "100px"
            :height "100px"
            :margin "10px"}
    :onClick (partial click-fn x y)}
   (if-let [piece-id (get-in game-state [:turns turn-num :derived/coords {:x x :y y}])]
     (let [color (:color (game/get-piece-at game-state turn-num {:x x :y y}))]
       [:div {:style {:width "100%"
                      :height "100%"}
              :class
              (cond
                (= coord selected-coord) "table-info"
                (= :white color) "table-secondary"
                :else "table-dark")
              :scope "Row"}
        (str \| x \space y \space piece-id \space (get-in game-state [:pieces piece-id :color]) \|)
        [:br]
        (get-in game-state [:derived/possibles piece-id])
        ])
     [:div
      (str \| x \space y " - -|")
      [:br]
      (str "- -")
      ])])
(defn display-board
  [game-state-atom display-state-atom]
   (let [{:keys [turn ?clicked] :as display-state} @display-state-atom
         game-state (assoc @game-state-atom :derived/coords (-> @game-state-atom :board (nth turn) constants/derived-coords))
         turns (game/get-turn-num game-state)
         click-fn (fn [x y] (click game-state-atom display-state-atom x y))
         game-won (game/game-won @game-state-atom)
         ]
     [:div
      (if game-won
        [:h2
        (case game-won
          :white "Game won by white!"
          :black "Game won by black!"
          :stalemate "Both teams lost their kings!")])
      [:div.grid
       {:style {:display :flex
                :flex-direction :column}}
       (doall (for [y (range (-> game-state :board-stats :width dec) -1 -1)]
                ^{:key y}
                [:div
                 {:style {:display :flex
                          :flex-direction :row}}
                 (doall (for [x (range 0 (-> game-state :board-stats :height))]
                          (display-square
                            game-state
                            (if (and
                                  (= turn turns)
                                  (not game-won))
                              click-fn) ; Only allow clicking on current turn, or if game not won
                            (:turn @display-state-atom)
                            {:x x :y y}
                            ?clicked
                            )))]))]]))

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
