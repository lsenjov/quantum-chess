(ns ^:figwheel-hooks quantum-chess.constants
  )

;; Each of the 6 pieces, ordered in ascending rarity,
;; So hopefully the algorithm can weed out a bunch of options early
(def all-pieces [:P :B :N :R :Q :K])
; Disabled because we're doing mini chess only
;(def side-totals {:K 1 :Q 1 :R 2 :N 2 :B 2 :P 8})
(def side-totals {:K 1 :Q 0 :R 1 :N 1 :B 1 :P 2})

(defn all-possibles
  [num-pieces]
  (->> side-totals
       (remove (fn [[_ v]] (zero? v)))
       (keys)
       (repeat)
       (map vector (range num-pieces))
       (into {})
       ))
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
(def blank-board {
                  :board-stats {:width 6 :height 6}
                  ;:pieces {0 {:color :white}}
                  :pieces (initial-pieces 6)
                  :derived/possibles {0 #{:K :Q :B :N :R :P} 1 #{:K :Q :B :N :R :P}}

                  ;:turns [{ :board {0 {:x 4 :y 5}} :player :white :move {:from {:x 0 :y 0} :to {:x 0 :y 1}} :derived/coords {{:x 4 :y 5} 0} }]
                  :turns [{:board (initial-positions 6 6 6)
                           :player :white
                           :derived/coords (derived-coords (initial-positions 6 6 6))}]

                  ; Pieces 0-15 are white, 16-31 are black
                  :piece-list all-pieces
                  :side-totals side-totals
                  })
