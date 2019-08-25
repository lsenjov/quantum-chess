(ns ^:figwheel-hooks quantum-chess.validator
  (:require [quantum-chess.constants :as constants]
            [quantum-chess.game :as game]
            [quantum-chess.generator :as generator]
  ))



(defn validate-history-turn
  [game-state turn history]
  (let [
      {:keys [from to]} (:move turn)
      piece-id          (game/get-piece-at game-state turn from)
      piece-type        (get history piece-id)]
    (game/valid-move-by-piece? game-state turn from to piece-type)))


(defn validate-history
  [game-state history]
  (reduce #(and %1 %2)  ;NEED TO MAKE THIS LAZY
    (map #(validate-history-turn game-state % history) (-> game-state :turns count range))))


;
;(defn update-possibles
;  [possible history]
;  (reduce-kv (fn [m k v] (assoc 
;
;
;(defn generate-possibles
;  [game-state old-possibles]
;  (reduce 
;    (fn [possible history]
;      )
;    {} 
;    (filter #(validate-history game-state %) (generator/generate-full-histories old-possibles))
;  )
;
