(ns ^:figwheel-hooks quantum-chess.generator
  (:require [quantum-chess.constants :as constants]
            [clojure.math.combinatorics :as combo]))
(def all-chess-types constants/all-pieces)
(def chess-type-counts constants/side-totals)

(def chess-piece-count
  (->> chess-type-counts vals (reduce +)))

(defn too-few-of
  "Are there less than chess-type-counts pieces?"
  [pieces chess-type]
  (<
    (count (filter #(= % chess-type) pieces))
    (chess-type-counts chess-type)))

(defn expand-team-history
  "for one player of mini chess"
  [pieces chess-type-list]
  (filter #(not (nil? %))
    (map 
      (fn [chess-type]
        (if (too-few-of pieces chess-type)
          (conj pieces chess-type)
          nil))
      chess-type-list)))

(defn generate-team-histories
  [root]
  (filter #(= (count %) chess-piece-count) (tree-seq
    #(not= chess-piece-count (count %))
    #(expand-team-history % all-chess-types)
    root
  )))

(defn generate-team-first-history
  []
  (first (generate-team-histories [])))


(def testpossibles {
  0 #{:K :B :N :R :P}
  1 #{:B :N :R}
  2 #{:K :N :R :P}
  3 #{:K :R :P}
  4 #{:K :B :N :P}
  5 #{:K :B :R}
})

(def reallyallpossibles {
  0 #{:K :B :N :R :P}
  1 #{:K :B :N :R :P}
  2 #{:K :B :N :R :P}
  3 #{:K :B :N :R :P}
  4 #{:K :B :N :R :P}
  5 #{:K :B :N :R :P}
})


(defn get-children
  [{:keys [depth node] :as parent} possibles]
  (map
    (fn [x] {:depth (inc depth) :node x})
    (expand-team-history node (get possibles depth))))

(defn generate-team-histories-known
  [possibles]
  (->> (tree-seq
         #(>= chess-piece-count (:depth %))
         #(get-children % possibles)
         {:depth 0 :node []})
       (filter #(= chess-piece-count (:depth %)))
       (map :node)))
