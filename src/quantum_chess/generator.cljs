(ns ^:figwheel-hooks quantum-chess.generator)


;(def all-chess-types (list :king :queen :rook :bishop :pawn))
;(def chess-type-counts {:king 1 :queen 1 :rook 1 :bishop 1 :pawn 2})
(def all-chess-types (list :P :B :N :R :Q :K))
(def chess-type-counts {:K 1 :Q 1 :R 2 :N 2 :B 2 :P 8})


(defn chess-piece-count
  []
  (->> chess-type-counts vals (reduce +)))

(defn too-few-of
  "Are there less than chess-type-counts pieces?"
  [pieces chess-type]
  (<
    (count (filter #(= % chess-type) pieces))
    (chess-type-counts chess-type)))

(defn expand-history
  "for one player of mini chess"
  [pieces chess-type-list]
  (filter #(not (nil? %))
    (map 
      (fn [chess-type]
        (if (too-few-of pieces chess-type)
          (conj pieces chess-type)
          nil))
      chess-type-list)))

(defn generate-histories
  "create all possible histories"
  [history]
  (if (= (chess-piece-count) (count history))
    (list history)
    (->> all-chess-types
      (expand-history history)
      (map generate-histories)
      ;(reduce concat))))
      )))


(defn generate-histories-from
  "create all possible histories"
  [history [current-chess-type & remaining-current-history :as current-history] ]
  (if (= (chess-piece-count) (count history))
    (list history)
    (concat
      (generate-histories-from
        (conj history current-chess-type)
        remaining-current-history)
      (->> all-chess-types
        (drop-while #(not= current-chess-type %))
        (rest)
        (expand-history history)
        (map generate-histories)
        (reduce concat)))))
     
(defn generate-first-history
  []
  (first (generate-histories [])))

(defn generate-next-history
  "Create the next history in the sequence of all possible histories"
  [current-history]
  (nth (generate-histories-from [] current-history) 1))

