(ns ^:figwheel-hooks quantum-chess.core
  (:require
   [goog.dom :as gdom]
   [reagent.core :as reagent :refer [atom]]))

(println "This text is printed from src/quantum_chess/core.cljs. Go ahead and edit it and see reloading in action.")

(defn multiply [a b] (* a b))


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
;; define your app data so that it doesn't get over-written on reload
(def blank-board {:board [{0 {:x 0 :y 0}
                           1 {:x 1 :y 0}
                           }]
                  ; Anything derived/* is derived in the validator
                  :derived/coords {{:x 0 :y 0} 0 {:x 1 :y 0} 1}
                  ;:pieces {0 {:color :white :possibles #{:K :Q :B :K :R :P}}}
                  :pieces {0 {:color :white}
                           1 {:color :black}}
                  :derived/possibles {0 #{:K :Q :B :N :R :P} 1 #{:K :Q :B :N :R :P}}
                  ;:history [{:from {:x 0 :y 0} :to {:x 0 :y 1}}]

                  ; Pieces 0-15 are white, 16-31 are black
                  :last-good side-start
                  :piecevalue-by-value (map-indexed (fn [idx idm] [idm idx]) all-pieces)
                  :piecevalue-by-piece (map-indexed (fn [idx idm] [idx idm]) all-pieces)
                  })
(def app-state (atom {:text "Test"}))
(def game-state-atom (atom blank-board))

(defn next-history
  "Given a game state, calculates the _next_ possible history. Returns a new game state
  Note that the :last-good in game-state may not be valid, we don't care, we just create a new one
  Throw an exception if we've hit the end and don't have any more items to check"
  [{:keys [last-good] :derived/keys [possibles] :as game-state}]
  game-state ;; TODO
  )

(defn get-app-element []
  (gdom/getElement "app"))

(defn display-square
  [game-state x y]
  [:td
  (if-let [piece-id (get-in game-state [:derived/coords {:x x :y y}])]
    (str \| x \space y \space piece-id \space (get-in game-state [:pieces piece-id :color]) \|)
    (str \| x \space y " - -|")
    )])

(defn hello-world []
  [:div
   [:table>tbody
    (doall (for [y (range 0 8)]
      [:tr (doall (for [x (range 0 8)] (display-square @game-state-atom x y)))]))]
   [:div (pr-str @game-state-atom)]
   ])

(defn mount [el]
  (reagent/render-component [hello-world] el))

(defn mount-app-element []
  (when-let [el (get-app-element)]
    (mount el)))

;; conditionally start your application based on the presence of an "app" element
;; this is particularly helpful for testing this ns without launching the app
(mount-app-element)

;; specify reload hook with ^;after-load metadata
(defn ^:after-load on-reload []
  (mount-app-element)
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
)
