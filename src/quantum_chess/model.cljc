(ns quantum-chess.model
  (:require [clojure.spec.alpha :as s]))

(s/def ::piece-type #{:K :Q :R :N :B :P})

(s/def :board/width integer?)
(s/def :board/height integer?)
(s/def :board/board-stats (s/keys :req-un [:board/width :board/height]))

(s/def :piece/color #{:white :black})
(s/def :game/piece (s/keys :req-un [:piece/color]))
(s/def :game/pieces (s/map-of integer? :game/piece))

(s/def :square/x integer?)
(s/def :square/y integer?)
(s/def :square/location (s/keys :req-un [:square/x :square/y]))
(s/def :turn/board (s/map-of integer? :square/location))
(s/def :turn/player :piece/color)
(s/def :move/from :square/location)
(s/def :move/to :square/location)
(s/def :turn/move (s/keys :req-un [:square/from :square/to]))
(s/def :derived/coords (s/map-of :square/location integer?))
(s/def :turn/turn (s/keys :req [:derived/coords]
                          :req-un [:turn/board :turn/player]
                          :opt-un [:turn/move]))
(s/def :turn/turns (s/coll-of :turn/turn))

(s/def :game/piece-list (s/coll-of ::piece-type))
(s/def :game/side-totals (s/map-of ::piece-type integer?))

(s/def :derived/possibles (s/map-of integer? (s/and
                                               set?
                                               (s/coll-of ::piece-type))))

(s/def ::game (s/keys :req-un [:board/board-stats
                               :game/pieces
                               :turn/turns
                               :game/piece-list
                               :game/side-totals]
                      :req [:derived/possibles]))

(s/def :generator/history (s/map-of integer? ::piece-type))
(s/def :generator/histories (s/coll-of :generator/history))