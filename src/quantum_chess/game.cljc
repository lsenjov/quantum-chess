(ns ^:figwheel-hooks quantum-chess.game
  "Controls the game and does movements"
  (:require
    [quantum-chess.constants :as constants]))

(defn get-player-turn
  "Returns :white or :black for the player to play _next"
  [game-state]
  (if (-> game-state :board count even?)
    :white
    :black))
(defn set-derived-fields-coords
  "Set coords derived field"
  [turn]
  (assoc turn :derived/coords (constants/derived-coords turn)))
(defn get-turn-num
  "Gets the last index available in turns (If there's 15 items, will return index 14)"
  [game-state]
  (-> game-state :turns count dec))
(defn get-board-at-turn
  [game-state turn-num]
  (-> game-state :turns (get turn-num) :board))
(defn set-derived-fields
  "Set _all_ the derived fields in a game map"
  ([game-state turn]
   (-> game-state
       (update-in game-state [:turns turn] set-derived-fields-coords)))
  ([game-state]
   (set-derived-fields game-state (get-turn-num game-state))))

(defn get-piece-at
  "Gets the piece at the gameboard
  Returns keys :id :color, or nil if no piece"
  ([game-state turn-num {:keys [x y] :as coord}]
   (if-let [id (-> game-state :turns (get turn-num) :derived/coords (get coord))]
     {:id id :color (-> game-state :pieces (get id) :color)}
     nil))
  ([game-state coord]
   (get-piece-at game-state (get-turn-num game-state) coord)))
(defn valid-coord?
  "Is the coordinate on the board?"
  ([game-state turn-num {:keys [x y] :as coord}]
   (let [{:keys [width height]} (-> game-state :board-stats)]
     (and
       (<= 0 x (dec width))
       (<= 0 x (dec height)))))
  ([game-state coord]
   (valid-coord? game-state (get-turn-num game-state) coord)))

(defn- abs
  [x]
  (if (< 0 x)
    x
    (- x)))
(defn- step-closer-int
  "Given two integers, return an integer one step closer"
  [from to]
  (cond
    (= from to) to
    (< from to) (inc from)
    :else (dec from)))
(defn- step-closer
  "Given two coords, returns a coord one step closer"
  [coord-from coord-to]
  {:x (step-closer-int (:x coord-from) (:x coord-to))
   :y (step-closer-int (:y coord-from) (:y coord-to))})
(defn- coords-between
  "Returns a list of coordinates between from and to"
  ([coord-from coord-to coords]
   (if (= coord-from coord-to)
     (butlast coords) ; Don't get the last coord which is where we are
     (let [new-coord (step-closer coord-from coord-to)]
       (coords-between new-coord coord-to (conj coords new-coord)))))
  ([coord-from coord-to]
   (coords-between coord-from coord-to [])))
(defn- any-piece?
  "Walks across a game board, returns true if any piece is on the walk, else false"
  ([game-state turn-num coords]
   (->> coords
        (map (partial get-piece-at game-state turn-num))
        (remove nil?)
        (count)
        (pos?)))
  ([game-state coords]
   (any-piece? game-state (get-turn-num game-state) coords)))
(defn- valid-orthagonal-move?
  ([game-state turn-num coord-from coord-to]
   (let [current-board (-> game-state :turns (get turn-num) :board)
         current-piece (-> game-state (get-piece-at turn-num coord-from))
         x-diff (- (:x coord-to) (:x coord-from))
         y-diff (- (:y coord-to) (:y coord-from))]
     (cond
       (and (not (zero? x-diff)) (and (zero? y-diff))) false
       (any-piece? game-state (coords-between coord-from coord-to)) false
       :otherwise true
       )))
  ([game-state coord-from coord-to]
   (valid-orthagonal-move? game-state (get-turn-num game-state) coord-from coord-to)))
(defn- valid-diagonal-move?
  ([game-state turn-num coord-from coord-to]
   (let [current-board (-> game-state :turns (get turn-num) :board)
         current-piece (-> game-state (get-piece-at turn-num coord-from))
         x-diff (- (:x coord-to) (:x coord-from))
         y-diff (- (:y coord-to) (:y coord-from))
         ]
     (cond
       (not= (abs x-diff) (abs y-diff)) false
       (any-piece? game-state (coords-between coord-from coord-to)) false
       :otherwise true
       )))
  ([game-state coord-from coord-to]
   (valid-diagonal-move? game-state (get-turn-num game-state) coord-from coord-to)))
(defn- valid-knight-move?
  [game-state turn-num coord-from coord-to]
  (let [current-board (-> game-state :turns (get turn-num) :board)
        current-piece (-> game-state (get-piece-at coord-from))]
    ; TODO
    ))

(defmulti valid-move-by-piece?
  "For the board as it is, is this a valid move?"
  (fn [game-state turn-num coord-from coord-to piece-type] piece-type))
(defmethod valid-move-by-piece? :default
  [game-state coord-from coord-to piece-type]
  false)
(defmethod valid-move-by-piece? :K
  [game-state turn-num coord-from coord-to piece-type]
  (let [current-board (get-board-at-turn game-state turn-num)
        current-piece (-> game-state (get-piece-at turn-num coord-from))
        ]
    (cond
      ; In bounds?
      (not (valid-coord? game-state turn-num coord-from)) false
      (not (valid-coord? game-state turn-num coord-to)) false
      ; Can't have stayed still
      (= coord-from coord-to) false
      ; King can't have moved more than one space
      (not (<= -1 (- (:x coord-from) (:x coord-to)) 1)) false
      (not (<= -1 (- (:y coord-from) (:y coord-to)) 1)) false
      ; If we're the same color as a piece we're moving to, we say no
      (-> game-state (get-piece-at turn-num coord-to) :color (= (:color current-piece))) false
      ; TODO king in check?
      :otherwise true
      )))
(defmethod valid-move-by-piece? :P
  [game-state turn-num coord-from coord-to piece-type]
  (let [current-board (get-board-at-turn game-state turn-num)
        current-piece (-> game-state (get-piece-at turn-num coord-from))]
    (cond
      ; In bounds?
      (not (valid-coord? game-state turn-num coord-from)) false
      (not (valid-coord? game-state turn-num coord-to)) false
      ; Can't have stayed still
      (= coord-from coord-to) false
      ; TODO moving
      ; If we're the same color as a piece we're moving to, we say no
      (-> game-state (get-piece-at turn-num coord-to) :color (= (:color current-piece))) false
      ; TODO en passant
      :otherwise true
      )))
(defmethod valid-move-by-piece? :R
  [game-state turn-num coord-from coord-to piece-type]
  (let [current-board (get-board-at-turn game-state turn-num)
        current-piece (-> game-state (get-piece-at turn-num coord-from))]
    (cond
      ; In bounds?
      (not (valid-coord? game-state turn-num coord-from)) false
      (not (valid-coord? game-state turn-num coord-to)) false
      ; Can't have stayed still
      (= coord-from coord-to) false
      (not (valid-orthagonal-move? game-state turn-num coord-from coord-to)) false
      ; If we're the same color as a piece we're moving to, we say no
      (-> game-state (get-piece-at turn-num coord-to) :color (= (:color current-piece))) false
      :otherwise true
      )))
(defmethod valid-move-by-piece? :Q
  [game-state turn-num coord-from coord-to piece-type]
  (let [current-board (get-board-at-turn game-state turn-num)
        current-piece (-> game-state (get-piece-at turn-num coord-from))]
    (cond
      ; In bounds?
      (not (valid-coord? game-state turn-num coord-from)) false
      (not (valid-coord? game-state turn-num coord-to)) false
      ; Can't have stayed still
      (= coord-from coord-to) false

      ; Queens can move either orthog or diagonal
      (and
        (not (valid-orthagonal-move? game-state turn-num coord-from coord-to))
        (not (valid-diagonal-move? game-state turn-num coord-from coord-to)))
      false

      ; If we're the same color as a piece we're moving to, we say no
      (-> game-state (get-piece-at turn-num coord-to) :color (= (:color current-piece))) false
      :otherwise true
      )))
(defmethod valid-move-by-piece? :N
  [game-state turn-num coord-from coord-to piece-type]
  (let [current-board (get-board-at-turn game-state turn-num)
        current-piece (-> game-state (get-piece-at coord-from))]
    (cond
      ; In bounds?
      (not (valid-coord? game-state turn-num coord-from)) false
      (not (valid-coord? game-state turn-num coord-to)) false
      ; Can't have stayed still
      (= coord-from coord-to) false
      ; TODO moving
      ; If we're the same color as a piece we're moving to, we say no
      (-> game-state (get-piece-at turn-num coord-to) :color (= (:color current-piece))) false
      :otherwise true
      )))
(defmethod valid-move-by-piece? :B
  [game-state turn-num coord-from coord-to piece-type]
  (let [current-board (get-board-at-turn game-state turn-num)
        current-piece (-> game-state (get-piece-at turn-num coord-from))]
    (cond
      ; In bounds?
      (not (valid-coord? game-state turn-num coord-from)) false
      (not (valid-coord? game-state turn-num coord-to)) false
      ; Can't have stayed still
      (= coord-from coord-to) false
      (not (valid-diagonal-move? game-state turn-num coord-from coord-to)) false
      ; If we're the same color as a piece we're moving to, we say no
      (-> game-state (get-piece-at turn-num coord-to) :color (= (:color current-piece))) false
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
        new-possibles (set (filter (partial valid-move-by-piece? game-state (get-turn-num game-state) coord-from coord-to) possibles))
        ]
    new-possibles
    ))
