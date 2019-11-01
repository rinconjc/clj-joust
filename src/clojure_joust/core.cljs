(ns ^:figwheel-hooks clojure-joust.core
  (:require [clojure-joust.model :as m :refer [app-state]]
            [clojure.string :as str]
            [goog.dom :as gdom]
            [oops.core :refer [ocall oget]]
            [reagent.core :as r :refer [atom]]))

(defn get-app-element []
  (gdom/getElement "app"))

(defn player-setup [player]
  (r/with-let [i (atom 0)]
    [:div
     [:img.character-pick {:src (m/avatars (:avatar @player))}]
     [:a.left {:href "#" :on-click #(m/change-avatar player i dec)}
      [:i.material-icons "chevron_left"]]
     [:a.right {:href "#" :on-click #(m/change-avatar player i inc)}
      [:i.material-icons "chevron_right"]]
     [:div.input-field.col.s10
      [:input.white-text {:value (:name @player)
                          :on-change #(swap! player assoc :name (-> % .-target .-value))}]]]))

(defn player-selection-page []
  [:div
   [:div.row
    [:div.col.s12.center
     [:h4 "Select Players"]]
    [:div.col.s5.m3.offset-m2
     [player-setup (r/cursor app-state [:player1])]]
    [:div.col.s2.center [:h3 "VS"]]
    [:div.col.s5.m3
     [player-setup (r/cursor app-state [:player2])]]]
   [:div.row
    [:div.col.s12
     [:div.center [:a.btn {:on-click m/start-game} "Start"]]]]])

(defn player [{:keys [x y avatar]}]
  [:image {:x x :y y :href (m/avatars avatar) :width m/avatar-width :height m/avatar-height}])

(defn handle-keys [e]
  (when-not (:ended? @app-state)
    (case (-> (oget e "key") str/lower-case)
      "l" (m/move-right :player2)
      "j" (m/move-left :player2)
      "i" (m/jump :player2)
      "a" (m/move-left :player1)
      "w" (m/jump :player1)
      "d" (m/move-right :player1)
      nil)))

(defn game-arena []
  [:div.center {:tab-index 0 :on-key-down handle-keys}
   [:svg.arena {:view-box "0 0 200 100" :height "80%" :width "95%" }
    [:rect {:x 25 :y 70 :width "150" :height "10" :fill "#555"}]
    [player (@app-state :player1)]
    [player (@app-state :player2)]]
   [:div.row
    [:div.col.s6
     [:h5 "ROUND:" (:round @app-state)]]
    [:div.col.s6
     [:h5 "SCORE " (-> @app-state :player1 (map [:name :score]) str/join) ":"
      (-> @app-state :player2 (map [:name :score]) str/join)]]]])

(defn main-page []
  [:div.box
   [:h4 "Clojure Joust!"
    [:img {:src "icon.png"}]]
   (condp instance? @app-state
     m/PlayerSelection [#'player-selection-page]
     m/Game [#'game-arena]
     [:p "Sorry can't display:" @app-state])])

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
