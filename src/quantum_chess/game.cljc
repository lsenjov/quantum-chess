(ns ^:figwheel-hooks quantum-chess.game
  "Controls the game and does movements"
  (:require
    [quantum-chess.constants :as constants]))

;; HELPERS
(def inverse-color
  {:white :black
   :black :white})
(defn get-player-turn
  "Returns :white or :black for the player to play _next"
  [game-state]
  (if (-> game-state :board count even?)
    :white
    :black))
(defn set-derived-fields-coords
  "Set coords derived field"
  [turn]
  (assoc turn :derived/coords (constants/derived-coords (:board turn))))
(defn get-turn-num
  "Gets the last index available in turns (If there's 15 items, will return index 14)"
  [game-state]
  (-> game-state :turns count dec))
(defn get-turn
  [game-state turn-num]
  (-> game-state :turns (get turn-num)))

(defn get-board-at-turn
  [game-state turn-num]
  (:board (get-turn game-state turn-num)))
(defn set-derived-fields
  "Set _all_ the derived fields in a game map"
  ([game-state turn]
   (update-in game-state [:turns turn] set-derived-fields-coords))
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
(defn- move-length
  "Returns the number of squares moved in the shortest path (including diagonals)
  0,0 -> 1,2 = 2"
  [coord-from coord-to]
  (->> (coords-between coord-from coord-to)
       count
       ; Coords-between only counts the intermediate steps, and is slightly too short
       inc))

;; PARTIAL MOVE VALIDATION
(defn- moved-forward?
  "Returns true if the piece has moved forward on the board"
  [coord-from coord-to {:keys [color] :as piece}]
  (let [y-diff (- (:y coord-to) (:y coord-from))]
    (cond
      (and (= :white color)  (pos? y-diff)) true
      (and (= :black color)  (neg? y-diff)) true
      :else false)))
(defn- valid-orthagonal-move?
  ([game-state turn-num coord-from coord-to]
   (let [current-board (-> game-state :turns (get turn-num) :board)
         current-piece (-> game-state (get-piece-at turn-num coord-from))
         x-diff (- (:x coord-to) (:x coord-from))
         y-diff (- (:y coord-to) (:y coord-from))]
     (cond
       (and (not (zero? x-diff)) (not (zero? y-diff))) false
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
        current-piece (-> game-state (get-piece-at turn-num coord-from))]
    ; If the piece has moved two spaces, AND it's not on a diagonal or orthag, it's a knight
    (and
      (not (valid-orthagonal-move? game-state turn-num coord-from coord-to))
      (not (valid-diagonal-move? game-state turn-num coord-from coord-to))
      (= 2 (move-length coord-from coord-to)))
    ; TODO
    ))

;; FULL MOVE VALIDATION
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
      ; Capturing
      (and
        (moved-forward? coord-from coord-to current-piece)
        (valid-diagonal-move? game-state turn-num coord-from coord-to)
        (= 1 (move-length coord-from coord-to))
        (-> game-state (get-piece-at turn-num coord-to) :color (= (inverse-color (:color current-piece))))) true
      ; TODO en passant

      ; Moving
      ; We're not capturing, so if there's a piece where we're going it's wrong
      (-> game-state (get-piece-at turn-num coord-to)) false
      (and
        (moved-forward? coord-from coord-to current-piece)
        (valid-orthagonal-move? game-state turn-num coord-from coord-to)
        (or
          (= 1 (move-length coord-from coord-to))
          (and
            (= 2 (move-length coord-from coord-to))
            ; TODO first move?
            ))) true
      :otherwise false)))
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
        current-piece (get-piece-at game-state turn-num coord-from)]
    (cond
      ; In bounds?
      (not (valid-coord? game-state turn-num coord-from)) false
      (not (valid-coord? game-state turn-num coord-to)) false
      ; Can't have stayed still
      (= coord-from coord-to) false
      ; TODO moving
      ; If we're the same color as a piece we're moving to, we say no
      (-> game-state (get-piece-at turn-num coord-to) :color (= (:color current-piece))) false

      (valid-knight-move? game-state turn-num coord-from coord-to) true
      :otherwise false
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

(defn move-piece
  "Moves a piece, dissasociates any piece that was already on the board in that place
  Returns a new turn, NOT a game state"
  [game-state coord-from coord-to]
  (let [current-board (get-turn game-state (get-turn-num game-state))
        current-piece-id (-> game-state (get-piece-at coord-from) :id)
        target-piece-id (-> game-state (get-piece-at coord-to) :id)
        turn-num (get-turn-num game-state)
        ]
    (-> current-board
        ; Remove the target piece
        (update-in [:board] dissoc target-piece-id)
        ; Move the piece
        (assoc-in [:board current-piece-id] coord-to)
        ; Set new derived coords
        (set-derived-fields-coords)
        )))
(defn make-move
  "Makes a move and saves the new board state. Does not validate correctness"
  [game-state coord-from coord-to]
  (let [game-state (set-derived-fields game-state)
        current-piece (-> game-state (get-piece-at coord-from))
        current-piece-id (:id current-piece)
        possibles (get-in game-state [:derived/possibles current-piece-id])
        turn-num (get-turn-num game-state)
        new-possibles (set (filter (partial valid-move-by-piece? game-state turn-num coord-from coord-to) possibles))
        ]
    new-possibles
    (if (zero? (count new-possibles))
      (throw (#?(:clj Exception. :cljs js/Error.) (str "That piece definitely can't move that way. Possibles: " possibles)))
      (-> game-state
          (assoc-in [:turns turn-num :move] {:from coord-from :to coord-to :id current-piece-id})
          (update-in [:turns] conj (move-piece game-state coord-from coord-to))
          (assoc-in [:derived/possibles current-piece-id] new-possibles)
          ))))

(defn- has-king?
  [game-state player]
  (as-> game-state v
    ; Get all pieces
    (:pieces v)
    ; Belonging to the player
    (filter (fn [[_ v]] (= player (:color v))) v)
    ; Form a set
    (set v)
    ; Get possibles of that player
    (filter (fn [[k pos-set]] (and (v k) (pos-set :K))) (:derived/possibles game-state))
    (not (empty? v))))
(defn game-won
  "Checks if sides no longer have pieces on the board that could be a king
  Returns the winner's colour
  Returns nil if both still do, :white, :black, or :stalemate if neither have kings"
  [game-state]
  (let [white-king? (has-king? game-state :white)
        black-king? (has-king? game-state :black)]
    (cond
      (and white-king? black-king?) nil
      white-king? :white
      black-king? :black
      :else :stalemate
      )))
