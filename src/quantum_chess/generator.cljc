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
    (count (filter (fn [[k v]] (= chess-type v)) pieces))
    (chess-type-counts chess-type)))

(defn -expand-team-history
  "for one player of mini chess"
  [pieces chess-type-list piece-id]
  (filter #(not (nil? %))
    (map 
      (fn [chess-type]
        (if (-too-few-of pieces chess-type)
          (assoc pieces piece-id chess-type)
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
  [{:keys [node remaining] :as parent} possibles]
  (let [[piece-id & remaining-tail] remaining] 
    (map
      (fn [x] {:node x :remaining remaining-tail})
      (-expand-team-history node (get possibles piece-id) piece-id))))

(defn generate-teamlocal-histories
  [teamlocal-possibles piece-ids]
  (->> (tree-seq
         #(-> % :remaining seq)
         #(-get-children % teamlocal-possibles)
         {:node {} :remaining piece-ids})
       (filter #(-> % :remaining empty?))
       (map :node)))

(defn generate-team-histories
  [possibles team]
  (if (= team :white)
    (generate-teamlocal-histories possibles (range 0 chess-piece-count))
    (generate-teamlocal-histories possibles (range chess-piece-count (* 2 chess-piece-count)))))
    
(defn generate-full-histories
  [possibles]
  (map (partial apply concat)
    (combo/cartesian-product
      (generate-team-histories possibles :white)
      (generate-team-histories possibles :black))))
  
(defn generate-all-full-histories
  []
  (generate-full-histories (constants/all-possibles (* 2 chess-piece-count))))
