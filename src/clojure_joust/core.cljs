(ns ^:figwheel-hooks clojure-joust.core
  (:require [clojure.string :as str]
            [goog.dom :as gdom]
            [oops.core :refer [ocall oget]]
            [reagent.core :as r :refer [atom]]))

(def ice-friction 0.16)              ;50/2secs: 50=5*120-0.5a120^2
(def gravity 0.25)
(def max-speed-x 3)
(def jump-speed-y -4)
(def avatar-max-y 40)
(def avatar-min-x 15)
(def avatar-max-x 165)
(def scene-max-y 100)
(def avatar-height 30)
(def avatar-width 25)

(def elasticity 0.8)
(def max-rounds 5)

(defn spy [v]
  (println v))
;; define your app data so that it doesn't get over-written on reload
(defrecord Game [player1 player2 round winner])

(defrecord State [game])

(defonce app-state (atom {:text "Clojure Joust!"
                          :player1 {:avatar :bandicoot :name "Player1" }
                          :player2 {:avatar :lycanroc :name "Player2" }
                          :round 1
                          :winner nil}))

(def bg-img "https://hdwallpaper20.com/wp-content/uploads/2016/05/waterfall-wallpaper-beautiful-waterfalls-hd-wallpaper-high-definition.jpg")

(def avatars {:bunny  "images/bugsbunny.png"
              :pea "images/peashooter.png"
              :bandicoot "images/bandicoot.png"
              :lycanroc "images/lycanroc.png"})

(def avatar-names (keys avatars))

(defn get-app-element []
  (gdom/getElement "app"))

(defn player-setup [player]
  (r/with-let [i (atom 0)
               max-i (dec (count avatar-names))]
    [:div
     [:img.character-pick {:src (avatars (@player :avatar))}]
     [:a.left {:href "#" :on-click (fn[] (swap! player assoc :avatar
                                                (nth avatar-names (swap! i #(if (zero? %) max-i (dec %))))))}
      [:i.material-icons "chevron_left"]]
     [:a.right {:href "#" :on-click (fn[] (swap! player assoc :avatar
                                                 (nth avatar-names (swap! i #(if (= % max-i) 0 (inc %)))))) }
      [:i.material-icons "chevron_right"]]
     [:input {:value (:name @player)
              :on-change #(swap! player assoc :name (-> % .-target .-value))}]]))

(declare start-game)

(defn player-selection []
  [:div.row
   [:div.col.s12.center
    [:h4 "Select Players"]]
   [:div.col.s5.m3.offset-m2.white
    [player-setup (r/cursor app-state [:player1])]]
   [:div.col.s2.center.middle.full-h "VS"]
   [:div.col.s5.m3.white
    [player-setup (r/cursor app-state [:player2])]]
   [:div.col.s12
    [:div.center [:a.btn {:on-click start-game} "Start"]]]])

;; (def game-time (atom nil))

(defn reset-players [state]
  (-> state (update :player1 assoc :x 25 :y 40 :velocity [0 0])
      (update :player2 assoc :x 150 :y 40 :velocity [0 0])))

(defn fallen? [player]
  (> (:y player) scene-max-y))


(defn plus [& vs]
  (into [] (apply map + vs)))

(defn minus [& vs]
  (into [] (apply map - vs)))

(defn times [v n]
  (into [] (map #(* % n) v)))

(defn friction [[velocity-x _]]
  (* (js/Math.sign velocity-x) -1 (min ice-friction (js/Math.abs velocity-x))))

(defn velocity-after-collision [v1 v2]
  (times (minus (times v2 (inc elasticity)) (times v1 (dec elasticity))) 0.5))

(defn bumped? [{x1 :x y1 :y v1 :velocity} {x2 :x y2 :y v2 :velocity}]
  (let [x-overlap (- avatar-width (Math/abs (- x1 x2)))
        y-overlap (- avatar-height (Math/abs (- y1 y2)))]
    (and (pos? x-overlap) (pos? y-overlap)
         (neg? (* (js/Math.sign (- x1 x2))
                  (- (first v1) (first v2)))))))

;; dist p1 p2: x1-x2 x10+v1t-(x20+v2t)= v1-v2;

(defn velocity-and-position [{:keys [x y velocity]}]
  (let [off-platform? (or (< x avatar-min-x) (> x avatar-max-x))
        on-platform? (and (not off-platform?) (>= y avatar-max-y))
        acceleration (if on-platform?
                       [(friction velocity) 0]
                       [0 gravity])
        velocity* (plus velocity acceleration)
        pos* (plus [x y] velocity (times acceleration 0.5))]
    (if on-platform?
      [(update velocity* 1 min 0)
       (update pos* 1 min avatar-max-y)]
      [velocity* pos*])))

(defn update-player [{:keys [x y velocity] :as  player}]
  (let [[velocity [x y]] (velocity-and-position player)]
    (assoc player :x x :y y :velocity velocity)))

(defn end-game [{:keys [player1 player2 :as state]}]
  (assoc state
         :ended? true
         :winner
         (if (> (:score player1) (:score player2)) player1 player2)))

(defn update-game [state]
  (let [[player1 player2] (map #(update-player (% state)) [:player1 :player2])
        round-winner (cond (fallen? (state :player1)) :player2
                           (fallen? (state :player2)) :player1)
        state (if round-winner
                (-> state
                    (update :round inc)
                    (update-in [round-winner :score] inc))
                state)]
    (if round-winner
      (if (= max-rounds (:round state))
        (end-game state)
        (reset-players state))
      (if (bumped? player1 player2)
        (assoc state
               :player1 (update player1 :velocity velocity-after-collision (:velocity player2))
               :player2 (update player2 :velocity velocity-after-collision (:velocity player1)))
        (assoc state :player1 player1 :player2 player2)))))

(defn animate []
  (ocall js/window "requestAnimationFrame"
         #(do (swap! app-state update-game)
              (animate))))

(defn start-game []
  (swap! app-state #(-> % (assoc :page :arena)
                        reset-players))
  (animate))



(defn player [{:keys [x y avatar]}]
  [:image {:x x :y y :href (avatars avatar) :width avatar-width :height avatar-height}])

(defn move-right [player]
  (swap! app-state update player update-in [:velocity 0] #(+ max-speed-x (min 0 %))))

(defn move-left [player]
  (swap! app-state update player update-in [:velocity 0] #(- (max 0 %) max-speed-x)))

(defn jump [player]
  (swap! app-state update player update-in [:velocity 1] #(if (zero? %) jump-speed-y %)))

(defn handle-keys [e]
  (when-not (:ended? @app-state)
    (case (-> (oget e "key") str/lower-case)
      "l" (move-right :player2)
      "j" (move-left :player2)
      "i" (jump :player2)
      "a" (move-left :player1)
      "w" (jump :player1)
      "d" (move-right :player1)
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

(def pages {:players #'player-selection
            :arena #'game-arena})

(defn main-page []
  [:div.box
   [:h4 (:text @app-state)
    [:img {:src "icon.png"}]]
   [(-> @app-state :page pages)]])

(defn mount [el]
  (r/render-component [main-page] el))

(defn mount-app-element []
  (swap! app-state update :page #(or % :players))
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
