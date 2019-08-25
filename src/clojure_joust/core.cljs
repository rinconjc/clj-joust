(ns ^:figwheel-hooks clojure-joust.core
  (:require
   [goog.dom :as gdom]
   [reagent.core :as reagent :refer [atom]]))

(println "This text is printed from src/clojure_joust/core.cljs. Go ahead and edit it and see reloading in action.")

(defn multiply [a b] (* a b))


;; define your app data so that it doesn't get over-written on reload
(defonce app-state (atom {:text "Clojure Joust!"
                          :player1 {:avatar :lion}
                          :player2 {:avatar :bear}}))

(def avatars {:lion "https://blog.udemy.com/wp-content/uploads/2014/04/shutterstock_15856930.jpg"
              :bear "https://www.exploringnature.org/wordpress/wp-content/uploads/2015/01/bear_standing_up.jpg"})

(defn get-app-element []
  (gdom/getElement "app"))

(defn player-selection []
  [:div.row
   [:div.col.s5.m3.offset-m2.white
    [:img.character-pick
     {:src ""}]
    [:a.left.arrow [:i.material-icons "chevron_left"]]
    [:a.arrow.right [:i.material-icons "chevron_right"]]
    [:span "Player 1"]]
   [:div.col.s2.center.middle.full-h "VS"]
   [:div.col.s5.m3.white
    [:img.character-pick
     {:src ""}]
    [:span "Player 2"]]])

(defn main-page []
  [:div.container
   [:h4 (:text @app-state)
    [:img {:src "icon.png"}]]
   [player-selection]])

(defn mount [el]
  (reagent/render-component [main-page] el))

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
