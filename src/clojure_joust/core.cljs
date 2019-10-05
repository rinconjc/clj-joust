(ns ^:figwheel-hooks clojure-joust.core
  (:require
   [goog.dom :as gdom]
   [reagent.core :as r :refer [atom]]
   [oops.core :refer [oget ocall oset!]]
   [clojure-joust.utils :as u]
   [clojure.string :as str]))

(def ice-friction -0.13)              ;50/2secs: 50=5*120-0.5a120^2
(def gravity 0.25)
(def max-speed-x 4)
(def jump-speed-y -4)
(def avatar-max-y 40)
(def avatar-min-x 15)
(def avatar-max-x 165)
(def scene-max-y 100)
(def avatar-height 30)
(def avatar-width 25)

(defn spy [v]
  (println v))
;; define your app data so that it doesn't get over-written on reload
(defonce app-state (atom {:text "Clojure Joust!"
                          :player1 {:avatar :bandicoot }
                          :player2 {:avatar :lycanroc }}))

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
     [:span name]]))

;; (def game-time (atom nil))

(defn reset-players [state]
  (-> state (update :player1 assoc :x 25 :y 40)
      (update :player2 assoc :x 150 :y 40)))

(defn fallen? [player]
  (> (:y player) scene-max-y))

(defn de-overlap [[x y] p2]
  (let [x-overlap (- avatar-width (Math/abs (- x (:x p2))))
        y-overlap (- avatar-height (Math/abs (- y (:y p2))))]
    (if (and (pos? x-overlap) (pos? y-overlap))
      (if (= y (:y p2))
        [(if (> x (:x p2)) (+ x x-overlap) (- x x-overlap)) y]
        [x (if (> y (:y p2)) (+ y y-overlap) (- y y-overlap))])
      [x y])))

(defn update-player [{:keys[speed-x speed-y x y] :as player} other-player]
  (let [off-platform (or (< x avatar-min-x) (> x avatar-max-x))
        on-platform (and (not off-platform) (= y avatar-max-y))
        [x* speed-x*] (if (zero? speed-x)
                        [x speed-x]
                        [(-> x (+ speed-x) ((if (pos? speed-x) + -) (* 0.5 ice-friction)))
                         (if (pos? speed-x) (max (+ speed-x ice-friction) 0)
                             (min (- speed-x ice-friction) 0))])
        [y* speed-y*] (if (and (zero? speed-y) on-platform)
                        [y speed-y]
                        [(min (+ y speed-y (* 0.5 gravity)) (if off-platform 200 avatar-max-y))
                         (if (and (pos? speed-y) on-platform) 0 (+ speed-y gravity))])
        [x* y*] (de-overlap [x* y*] other-player)]
    (assoc player :speed-x speed-x* :x x* :speed-y speed-y* :y y*)))

(defn update-game [state]
  (cond-> state
    true (update :player1 update-player (:player2 state))
    true (update :player2 update-player (:player1 state))
    (or (fallen? (state :player1)) (fallen? (state :player2))) (reset-players)))

(defn animate []
  (ocall js/window "requestAnimationFrame"
         #(do (swap! app-state update-game)
              (animate))))

(defn start-game []
  (swap! app-state #(-> % (assoc :page :arena)
                        reset-players))
  (animate))

(defn player-selection []
  [:div.row
   [:div.col.s5.m3.offset-m2.white
    [player-setup (r/cursor app-state [:player1])]]
   [:div.col.s2.center.middle.full-h "VS"]
   [:div.col.s5.m3.white
    [player-setup (r/cursor app-state [:player2])]]
   [:div.col.s12
    [:div.center [:a.btn {:on-click start-game} "Start"]]]])

(defn player [{:keys [x y avatar]}]
  [:image {:x x :y y :href (avatars avatar) :width avatar-width :height avatar-height}])

(defn move-right [player]
  (swap! app-state update player update :speed-x #(+ max-speed-x (min 0 %))))

(defn move-left [player]
  (swap! app-state update player update :speed-x #(- (max 0 %) max-speed-x)))

(defn jump [player]
  (swap! app-state update player update :speed-y #(if (zero? %) jump-speed-y %)))

(defn handle-keys [e]
  (case (-> (oget e "key") str/lower-case)
    "l" (move-right :player2)
    "j" (move-left :player2)
    "i" (jump :player2)
    "a" (move-left :player1)
    "w" (jump :player1)
    "d" (move-right :player1)
    nil))

(defn game-arena []
  [:div {:tab-index 0 :on-key-down handle-keys}
   [:svg.arena {:view-box "0 0 200 100" :height "450px" :width "900px" }
    [:rect {:x 25 :y 70 :width "150" :height "10" :fill "#555"}]
    [player (@app-state :player1)]
    [player (@app-state :player2)]]])

(defn setup [])

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
