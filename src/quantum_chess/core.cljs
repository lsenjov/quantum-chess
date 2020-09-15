(ns ^:figwheel-hooks quantum-chess.core
  (:require
   [goog.dom :as gdom]
   [reagent.core :as reagent :refer [atom]]
   [reagent.dom :as rd]
   
   [quantum-chess.validator]
   [quantum-chess.generator]
   [quantum-chess.constants :as constants]
   [quantum-chess.visualisation :as vis]
   [quantum-chess.game :as game]
   ))

(def app-state (atom {:text "Test"}))
(defonce game-state-atom (atom constants/blank-board))
(defonce display-state-atom (atom {:turn 0}))

(defn get-app-element []
  (gdom/getElement "app"))


(defn hello-world []
  [:div
   [vis/display-board game-state-atom display-state-atom]
   [vis/display-slider game-state-atom display-state-atom]
   [:button.btn.btn-primary
    {:onClick #(do
                 (reset! display-state-atom {:turn 0})
                 (reset! game-state-atom constants/blank-board)
                 )}
    "RESET"]
   [:div (pr-str @game-state-atom)]
   ])

(defn mount [el]
  (rd/render [hello-world] el))

(defn mount-app-element []
  (when-let [el (get-app-element)]
    (mount el)))

;; conditionally start your application based on the presence of an "app" element
;; this is particularly helpful for testing this ns without launching the app
(defn init!
      []
      (mount-app-element))

;; specify reload hook with ^;after-load metadata
(defn ^:after-load on-reload []
  (mount-app-element)
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
)

