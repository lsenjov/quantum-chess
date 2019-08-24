(ns ^:figwheel-hooks quantum-chess.generator)


(def all-chess-types (list :P :B :N :R :Q :K))
(def chess-type-counts {:K 1 :Q 1 :R 2 :N 2 :B 2 :P 8})


(def chess-piece-count
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
  [root]
  (filter #(= (count %) chess-piece-count) (tree-seq
    #(not= chess-piece-count (count %))
    #(expand-history % all-chess-types)
    root
  )))
    
(defn generate-first-history
  []
  (first (generate-histories [])))
