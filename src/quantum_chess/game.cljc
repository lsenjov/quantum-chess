(ns ^:figwheel-hooks quantum-chess.game
  "Controls the game and does movements"
  (:require
    [quantum-chess.constants :as constants]))

(defn get-player-turn
  "Returns :white or :black for the player to play _next"
  [game-state]
  (if (-> game-state :board count odd?)
    :white
    :black))
(defn set-derived-fields-coords
  "Set coords derived field"
  [game-state]
  (assoc game-state :derived/coords (-> game-state :board last constants/derived-coords)))
(defn set-derived-fields
  "Set _all_ the derived fields in a game map"
  [game-state]
  (-> game-state
      (set-derived-fields-coords)))

(defn get-piece-at
  "Gets the piece at the gameboard
  Returns keys :id :colour, or nil if no piece"
  [game-state {:keys [x y] :as coord}]
  (if-let [id (-> game-state :derived/coords coord)]
    {:id id :color (-> game-state :pieces (get id) :color)}
    nil))
(defn valid-coord?
  "Is the coordinate on the board?"
  [game-state {:keys [x y] :as coord}]
  (let [{:keys [width height]} (-> game-state :board-stats)]
    (and
      (<= 0 x (dec width))
      (<= 0 x (dec height)))))
(defmulti valid-move-by-piece?
  "For the board as it is, is this a valid move?"
  (fn [game-state coord-from coord-to piece-type] piece-type))
(defmethod valid-move-by-piece? :default
  [game-state coord-from coord-to piece-type]
  false)

(defmethod valid-move-by-piece? :K
  [game-state coord-from coord-to piece-type]
  (let [current-board (-> game-state :board last)
        current-piece (-> game-state (get-piece-at coord-from))
        ]
    (cond
      ; In bounds?
      (not (valid-coord? game-state coord-from)) false
      (not (valid-coord? game-state coord-to)) false
      ; Can't have stayed still
      (= coord-from coord-to) false
      ; King can't have moved more than one space
      (not (<= -1 (- (:x coord-from) (:x coord-to)) 1))
      (not (<= -1 (- (:y coord-from) (:y coord-to)) 1))
      ; TODO capturing
      ; TODO king in check?
      :otherwise true
      )))
(defmethod valid-move-by-piece? :P
  [game-state coord-from coord-to piece-type]
  (let [current-board (-> game-state :board last)
        current-piece (-> game-state (get-piece-at coord-from))]
    (cond
      ; In bounds?
      (not (valid-coord? game-state coord-from)) false
      (not (valid-coord? game-state coord-to)) false
      ; Can't have stayed still
      (= coord-from coord-to) false
      ; TODO moving
      ; TODO capturing
      ; TODO en passant
      :otherwise true
      )))
(defmethod valid-move-by-piece? :R
  [game-state coord-from coord-to piece-type]
  (let [current-board (-> game-state :board last)
        current-piece (-> game-state (get-piece-at coord-from))]
    (cond
      ; In bounds?
      (not (valid-coord? game-state coord-from)) false
      (not (valid-coord? game-state coord-to)) false
      ; Can't have stayed still
      (= coord-from coord-to) false
      ; TODO moving
      ; TODO capturing
      ; TODO en passant
      :otherwise true
      )))
(defmethod valid-move-by-piece? :Q
  [game-state coord-from coord-to piece-type]
  (let [current-board (-> game-state :board last)
        current-piece (-> game-state (get-piece-at coord-from))]
    (cond
      ; In bounds?
      (not (valid-coord? game-state coord-from)) false
      (not (valid-coord? game-state coord-to)) false
      ; Can't have stayed still
      (= coord-from coord-to) false
      ; TODO moving
      ; TODO capturing
      ; TODO en passant
      :otherwise true
      )))
(defmethod valid-move-by-piece? :N
  [game-state coord-from coord-to piece-type]
  (let [current-board (-> game-state :board last)
        current-piece (-> game-state (get-piece-at coord-from))]
    (cond
      ; In bounds?
      (not (valid-coord? game-state coord-from)) false
      (not (valid-coord? game-state coord-to)) false
      ; Can't have stayed still
      (= coord-from coord-to) false
      ; TODO moving
      ; TODO capturing
      ; TODO en passant
      :otherwise true
      )))
(defmethod valid-move-by-piece? :B
  [game-state coord-from coord-to piece-type]
  (let [current-board (-> game-state :board last)
        current-piece (-> game-state (get-piece-at coord-from))]
    (cond
      ; In bounds?
      (not (valid-coord? game-state coord-from)) false
      (not (valid-coord? game-state coord-to)) false
      ; Can't have stayed still
      (= coord-from coord-to) false
      ; TODO moving
      ; TODO capturing
      ; TODO en passant
      :otherwise true
      )))

(defn make-move
  "Makes a move and saves the new board state. Does not validate correctness"
  [game-state coord-from coord-to]

  )
