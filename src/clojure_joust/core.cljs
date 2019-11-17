(ns ^:figwheel-hooks clojure-joust.core
  (:require [clojure-joust.model :as m :refer [app-state]]
            [clojure.string :as str]
            [goog.dom :as gdom]
            [oops.core :refer [ocall oget]]
            [reagent.core :as r :refer [atom]]))

(defn get-app-element []
  (gdom/getElement "app"))

(defn player-setup [player face]
  (r/with-let [i (atom 0)
               max-i (dec (count m/avatar-names))]
    [:div
     [:img.character-pick {:src (m/avatar-image (:avatar @player) face)}]
     [:a.left {:href "#" :on-click (fn[] (swap! player assoc :avatar
                                                (nth m/avatar-names (swap! i #(if (zero? %) max-i (dec %))))))}
      [:i.material-icons "chevron_left"]]
     [:a.right {:href "#" :on-click (fn[] (swap! player assoc :avatar
                                                 (nth m/avatar-names (swap! i #(if (= % max-i) 0 (inc %)))))) }
      [:i.material-icons "chevron_right"]]
     [:input.white-text {:value (:name @player)
                         :on-change #(swap! player assoc :name (-> % .-target .-value))}]]))

(defn player-selection-page []
  [:div
   [:div.row
    [:div.col.s12.center
     [:h4 "Select Players"]]
    [:div.col.s5.m3.offset-m2
     [player-setup (r/cursor app-state [:player1]) :right]]
    [:div.col.s2.center [:h3 "VS"]]
    [:div.col.s5.m3
     [player-setup (r/cursor app-state [:player2]) :left]]]
   [:div.row
    [:div.col.s12
     [:div.center [:a.btn {:on-click m/start-game} "Start"]]]]])

(defn player [{:keys [x y avatar face]}]
  [:image {:x x :y y :href (m/avatar-image avatar face)
           :width m/avatar-width
           :height m/avatar-height}])

(defn handle-keys [e]
  (let [key (-> (oget e "key") str/lower-case)]
    (if (:ended? @app-state)
      (case key
        " " (m/start-game)
        nil)
      (case key
          "l" (m/move-right :player2)
          "j" (m/move-left :player2)
          "i" (m/jump :player2)
          "a" (m/move-left :player1)
          "w" (m/jump :player1)
          "d" (m/move-right :player1)
          nil))))

(defn player-score [{:keys [name score]}]
  (str name " : " (:rounds score) "/" (:points score)))

(defn winner-banner [game]
  [:text.banner {:x 25 :y 40 :text-length 150} "Winner  is  "  (-> ((game :winner) game) :name)])

(defn game-arena []
  [:div.center {:tab-index 0 :on-key-down handle-keys}
   [:svg.arena {:view-box "0 0 200 100" :height "80%" :width "95%" }
    [:rect {:x 25 :y 70 :width "150" :height "10" :fill "#555"}]
    [player (:player1 @app-state)]
    [player (:player2 @app-state)]
    (when (:ended? @app-state)
      [winner-banner @app-state])]
   [:div.row
    [:div.col.s4
     [:h5 "ROUND:" (:round @app-state)]]
    [:div.col.s2 [:h5 "SCORE"]]
    [:div.col.s3 [:h5 (player-score (:player1 @app-state))]]
    [:div.col.s3 [:h5 (player-score (:player2 @app-state))]]]])

;; model views
(defmulti view type)

(defmethod view m/PlayerSelection [state]
  [player-selection-page])

(defmethod view m/Game [state]
  [game-arena])

(defmethod view :default [state]
  [:p "no view for " state])

(defn main-page []
  [:div.box
   [:h4 [:a.brand-logo {:href "#" :on-click m/home} "Clojure Joust!"]
    [:img {:src "icon.png"}]]
   (view @app-state)])

(defn mount [el]
  (r/render-component [main-page] el))

(defn mount-app-element []
  (m/init)
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
