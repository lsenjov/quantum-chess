(ns ^:figwheel-hooks quantum-chess.validator
  (:require [quantum-chess.constants :as constants]
            [quantum-chess.game :as game]
            [quantum-chess.generator :as generator]
  ))



;(defn validate-history-turn
;  "Validate whether a history permits the move made in a given turn"
;  [game-state turn history]
;  (if (:move turn)
;    (let [
;        {:keys [from to]} (:move turn)
;        piece-id          (game/get-piece-at game-state turn from)
;        piece-type        (get history piece-id)]
;      (game/valid-move-by-piece? game-state turn from to piece-type))
;    true) ; turns without a move are always valid

(defn validate-history-turn
  "Validate whether a history permits the move made in a given turn"
  [game-state turn-num history]
  (let [turn (-> game-state :turns (get turn-num) :move)]
    (if (:move turn)
      (let [
          {:keys [from to]} (:move turn)
          piece-id          (game/get-piece-at game-state turn-num from)
          piece-type        (get history piece-id)]
        (game/valid-move-by-piece? game-state turn-num from to piece-type))
      true))) ; turns without a move are always valid


(defn validate-history
  [game-state history]
  ;(reduce #(and %1 %2)  ;NEED TO MAKE THIS LAZY
  (some
    #(validate-history-turn game-state % history)
    (-> game-state :turns count range)))


(defn update-possibles-item
  [possibles piece new-chess-type]
  (let [current-chess-types (get possibles piece)]
    (assoc possibles piece
      (set (conj current-chess-types new-chess-type)))))

(defn update-possibles
  [possibles history]
  (reduce-kv update-possibles-item possibles history))

(defn generate-possibles
  [game-state old-possibles]
  (reduce update-possibles {}
    (filter #(validate-history game-state %) (generator/generate-all-full-histories game-state))))


(def -test-game {
      :board-stats {:width 6 :height 6}
      :pieces {0 {:color :white} 1 {:color :white} 2 {:color :black} 3 {:color :black}}
      :derived/possibles {0 #{:P :B :N :R :Q :K} 1 #{:P :B :N :R :Q :K} 2 #{:P :B :N :R :Q :K} 3 #{:P :B :N :R :Q :K}}

      :turns [{:board {0 {:x 4 :y 5} 1 {:x 1 :y 1} 2 {:x 2 :y 3} 3 {:x 0 :y 5}}
               :player :white
               :derived/coords (constants/derived-coords {0 {:x 4 :y 5} 1 {:x 1 :y 1} 2 {:x 2 :y 3} 3 {:x 0 :y 5}})}]

      :piece-list '(0 1 2 3)
      :side-totals {:B 1 :R 1}
      })
