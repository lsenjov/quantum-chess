(ns ^:figwheel-hooks quantum-chess.core
  (:require
   [goog.dom :as gdom]
   [reagent.core :as reagent :refer [atom]]
   
   [quantum-chess.validator]
   [quantum-chess.generator]
   [quantum-chess.constants :as constants]
   [quantum-chess.visualisation :as vis]
   ))

(def app-state (atom {:text "Test"}))
(def game-state-atom (atom constants/blank-board))
(def display-state-atom (atom {:turn 0}))

(defn get-app-element []
  (gdom/getElement "app"))


(defn hello-world []
  [:div
   (vis/display-board @game-state-atom display-state-atom)
   (vis/display-slider @game-state-atom display-state-atom)
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
