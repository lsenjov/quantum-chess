(ns ^:figwheel-hooks quantum-chess.generator
  (:require [quantum-chess.constants :as constants]
            [clojure.math.combinatorics :as combo]))
(def all-chess-types constants/all-pieces)
(def chess-type-counts constants/side-totals)

(def chess-piece-count
  (->> chess-type-counts vals (reduce +)))

(defn -too-few-of
  "Are there less than chess-type-counts pieces?"
  [pieces chess-type]
  (<
    (count (filter #(= % chess-type) pieces))
    (chess-type-counts chess-type)))

(defn -expand-team-history
  "for one player of mini chess"
  [pieces chess-type-list]
  (filter #(not (nil? %))
    (map 
      (fn [chess-type]
        (if (-too-few-of pieces chess-type)
          (conj pieces chess-type)
          nil))
      chess-type-list)))

(def -testpossibles {
  0  #{:K :B :N :R :P}
  1  #{:B :N :R}
  2  #{:K :N :R :P}
  3  #{:K :R :P}
  4  #{:K :B :N :P}
  5  #{:K :B :R}
  6  #{:K :B :N :R :P}
  7  #{:K :N :R :P}
  8  #{:K :R :P}
  9  #{:K :B :N :P}
  10 #{:K :B :R}
  11  #{:K :N :R :P}
})


(def -reallyallpossibles {
  0  #{:K :B :N :R :P}
  1  #{:K :B :N :R :P}
  2  #{:K :B :N :R :P}
  3  #{:K :B :N :R :P}
  4  #{:K :B :N :R :P}
  5  #{:K :B :N :R :P}
  6  #{:K :B :N :R :P}
  7  #{:K :B :N :R :P}
  8  #{:K :B :N :R :P}
  9  #{:K :B :N :R :P}
  10 #{:K :B :N :R :P}
  11 #{:K :B :N :R :P}
})


(defn -get-children
  [{:keys [depth node] :as parent} possibles]
  (map
    (fn [x] {:depth (inc depth) :node x})
    (-expand-team-history node (get possibles depth))))

(defn generate-teamlocal-histories
  [teamlocal-possibles]
  (->> (tree-seq
         #(>= chess-piece-count (:depth %))
         #(-get-children % teamlocal-possibles)
         {:depth 0 :node []})
       (filter #(= chess-piece-count (:depth %)))
       (map :node)))

(defn generate-team-histories
  [possibles team]
  (if (= team :white)
    (generate-teamlocal-histories possibles)
    (generate-teamlocal-histories
      (reduce-kv (fn [m k v] (assoc m (- k chess-piece-count) v)) {} possibles))))
    
(defn generate-full-histories
  [possibles]
  (map (partial apply concat)
    (combo/cartesian-product
      (generate-team-histories possibles :white)
      (generate-team-histories possibles :black))))
  
(defn generate-all-full-histories
  []
  (generate-full-histories (constants/all-possibles (* 2 chess-piece-count))))
