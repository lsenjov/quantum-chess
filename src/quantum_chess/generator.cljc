(ns ^:figwheel-hooks quantum-chess.generator
  (:require [quantum-chess.constants :as constants]
            [clojure.math.combinatorics :as combo]))

(def all-chess-types constants/all-pieces)
;(def chess-type-counts constants/side-totals)

(defn side-total-count
  [side-totals]
  (->> side-totals vals (reduce +)))

;(def chess-piece-count
;  (->> chess-type-counts vals (reduce +)))

(defn- too-few-of
  "Are there less than chess-type-counts pieces?"
  [pieces chess-type side-totals]
  (<
    (count (filter (fn [[k v]] (= chess-type v)) pieces))
    (side-totals chess-type)))

(defn- expand-team-history
  "for one player of mini chess"
  [pieces chess-type-list piece-id side-totals]
  (filter #(not (nil? %))
    (map 
      (fn [chess-type]
        (if (too-few-of pieces chess-type side-totals)
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


(defn- get-children
  [{:keys [node remaining] :as parent} possibles side-totals]
  (let [[piece-id & remaining-tail] remaining] 
    (map
      (fn [x] {:node x :remaining remaining-tail})
      (expand-team-history node (get possibles piece-id) piece-id side-totals))))

(defn generate-teamlocal-histories
  [game possibles piece-ids]
  (->> (tree-seq
         #(-> % :remaining seq)
         #(get-children % possibles (:side-totals game))
         {:node {} :remaining piece-ids})
       (filter #(-> % :remaining empty?))
       (map :node)))

(defn generate-team-histories
  [game possibles team]
  (if (= team :white)
    (generate-teamlocal-histories game possibles (constants/team-pieces game :white))
    (generate-teamlocal-histories game possibles (constants/team-pieces game :black))))
    
(defn generate-full-histories
  [game possibles]
  (map (partial apply merge)
    (combo/cartesian-product
      (generate-team-histories game possibles :white) 
      (generate-team-histories game possibles :black))))
  
(defn generate-all-full-histories
  [game]
  (generate-full-histories game (:derived/possibles game)))
