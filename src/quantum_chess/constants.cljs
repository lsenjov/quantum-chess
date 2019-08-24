(ns ^:figwheel-hooks quantum-chess.constants
  )

;; Each of the 6 pieces, ordered in ascending rarity,
;; So hopefully the algorithm can weed out a bunch of options early
(def all-pieces [:P :B :N :R :Q :K])
(def side-totals {:K 1 :Q 1 :R 2 :N 2 :B 2 :P 8})
;(def side-start (repeat (concat (for [[k v] side-totals] (repeat k v))) 2))
(def side-start
  (apply concat
         (repeat 2
                 (apply concat
                        (for [p (reverse all-pieces)]
                          (repeat (get side-totals p) p))))))
(defn initial-positions
  [pieces-per-side width height]
  (->> (concat
         (map vector
              (range 0 pieces-per-side)
              (for [y (range 0 height) x (range 0 width)] {:x x :y y}))
         (map vector
              (range pieces-per-side (* 2 pieces-per-side))
              (for [y (range (dec height) 0 -1) x (range 0 width)] {:x x :y y})))
       (into {})))
(defn derived-coords
  [board]
  (reduce-kv (fn [m k v] (assoc m v k)) {} board))

(defn initial-pieces
  [pieces-per-side]
  (->> (concat
         (map vector (range 0 pieces-per-side) (repeat {:color :white}))
         (map vector (range pieces-per-side (* 2 pieces-per-side)) (repeat {:color :black})))
       (into {})
       ))
(def blank-board {:board [(initial-positions 16 8 8)]
                  ; Anything derived/* is derived in the validator
                  :derived/coords (derived-coords (initial-positions 16 8 8))
                  ;:pieces {0 {:color :white :possibles #{:K :Q :B :K :R :P}}}
                  :pieces (initial-pieces 16)
                  :derived/possibles {0 #{:K :Q :B :N :R :P} 1 #{:K :Q :B :N :R :P}}
                  ;:derived/history [{:from {:x 0 :y 0} :to {:x 0 :y 1}}]

                  ; Pieces 0-15 are white, 16-31 are black
                  :last-good side-start
                  :piecevalue-by-value (map-indexed (fn [idx idm] [idm idx]) all-pieces)
                  :piecevalue-by-piece (map-indexed (fn [idx idm] [idx idm]) all-pieces)
                  })
