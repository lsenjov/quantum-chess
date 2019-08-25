(ns ^:figwheel-hooks quantum-chess.control
  (:require
    [quantum-chess.validator]
    [quantum-chess.constants :as constants]
    [quantum-chess.game :as game]
    ))

(defn move-baby-move
  "Tries to move a piece, then validates the entire game-state
  
  Throws an exception if an invalid move, else returns the new game state"
  [game-state coord-from coord-to]
  (let [new-game-state (game/make-move game-state coord-from coord-to)
        validated-possibles (quantum-chess.validator/generate-possibles new-game-state)]
    (if (empty? validated-possibles)
      (do
        (println (pr-str new-game-state))
        (throw (#?(:clj Exception. :cljs js/Error.) (str "That piece definitely can't move that way. History validation failed"))))
      (assoc new-game-state :derived/possibles validated-possibles))))
