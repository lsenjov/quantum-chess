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
  Returns keys :id :color, or nil if no piece"
  [game-state {:keys [x y] :as coord}]
  (if-let [id (-> game-state :derived/coords (get coord))]
    {:id id :color (-> game-state :pieces (get id) :color)}
    nil)
  )
(defn valid-coord?
  "Is the coordinate on the board?"
  [game-state {:keys [x y] :as coord}]
  (let [{:keys [width height]} (-> game-state :board-stats)]
    (and
      (<= 0 x (dec width))
      (<= 0 x (dec height)))))

(defn- valid-orthagonal-move?
  [game-state coord-from coord-to piece-type]
  (let [current-board (-> game-state :board last)
        current-piece (-> game-state (get-piece-at coord-from))]
    
    ))
(defn- valid-diagonal-move?
  [game-state coord-from coord-to piece-type]
  (let [current-board (-> game-state :board last)
        current-piece (-> game-state (get-piece-at coord-from))]
    
    ))
(defn- valid-knight-move?
  [game-state coord-from coord-to piece-type]
  (let [current-board (-> game-state :board last)
        current-piece (-> game-state (get-piece-at coord-from))]
    
    ))
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
      (not (<= -1 (- (:x coord-from) (:x coord-to)) 1)) false
      (not (<= -1 (- (:y coord-from) (:y coord-to)) 1)) false
      ; If we're the same color as a piece we're moving to, we say no
      (-> game-state (get-piece-at coord-to) :color (= (:color current-piece))) false
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
      ; If we're the same color as a piece we're moving to, we say no
      (-> game-state (get-piece-at coord-to) :color (= (:color current-piece))) false
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
      ; If we're the same color as a piece we're moving to, we say no
      (-> game-state (get-piece-at coord-to) :color (= (:color current-piece))) false
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
      ; If we're the same color as a piece we're moving to, we say no
      (-> game-state (get-piece-at coord-to) :color (= (:color current-piece))) false
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
      ; If we're the same color as a piece we're moving to, we say no
      (-> game-state (get-piece-at coord-to) :color (= (:color current-piece))) false
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
      ; If we're the same color as a piece we're moving to, we say no
      (-> game-state (get-piece-at coord-to) :color (= (:color current-piece))) false
      :otherwise true
      )))

(defn make-move
  "Makes a move and saves the new board state. Does not validate correctness"
  [game-state coord-from coord-to]
  (let [game-state (set-derived-fields game-state)
        current-board (-> game-state :board last)
        current-piece (-> game-state (get-piece-at coord-from))
        current-piece-id (:id current-piece)
        possibles (get-in game-state [:derived/possibles current-piece-id])
        new-possibles (set (filter (partial valid-move-by-piece? game-state coord-from coord-to) possibles))
        ]
    new-possibles
    ))
