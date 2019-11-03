(ns clojure-joust.model
  (:require
   [reagent.core :as r :refer [atom]]
   [oops.core :refer [ocall oget]]))

;; Constants
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
(def max-rounds 4)

(def avatars* {:bunny {:image  "images/bugsbunny" }
               :pea {:image "images/peashooter" }
               :bandicoot {:image "images/bandicoot" }
               :lycanroc {:image "images/lycanroc" }
               :king-boo {:image "images/king-boo" }
               :gooigi {:image "images/gooigi" }
               :bull-eye {:image "images/bulleye"}})

(def avatars (into {} (for [[k v] avatars*] [k (:image v)])))

(def avatar-names (keys avatars))

(defn avatar-image [avatar face]
  (some-> avatars* avatar :image (str "-" (name face) ".png" )))
;; Model
(defrecord Game [player1 player2 round winner])
(defrecord Player [name avatar score])
(defrecord PlayerSelection [player1 player2])

(defn mk-player [name avatar]
  ;; (->Player name avatar 0)
  {:name name :avatar avatar :score 0})

(def default-game
  (map->Game {:player1 (mk-player "Player1" :bandicoot)
              :player2 (mk-player "Player2" :lycanroc)
              :round 1
              :winner nil}))

(def initial-state
  (->PlayerSelection (mk-player "Player1" :bandicoot) (mk-player "Player2" :lycanroc)))

(defn new-game [player1 player2]
  (->Game (assoc player1 :score 0) (assoc player2 :score 0) 1 nil))

(defonce app-state (atom nil))

(defn reset-players [state]
  (let [reset-fn (fn [player x face]
                   (as-> player p
                     (assoc p :x x :y 40 :velocity [0 0] :face face)))]
    (-> state
        (update :player1 reset-fn 25 :right)
        (update :player2 reset-fn 150 :left))))

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

(defn end-game [{:keys [player1 player2] :as state}]
  (assoc state
         :ended? true
         :winner
         (if (> (:score player1) (:score player2)) player1 player2)))

(defn update-game [state]
  (let [[player1 player2] (map #(update-player (% state)) [:player1 :player2])
        round-winner (cond (fallen? (:player1 state)) :player2
                           (fallen? (:player2 state)) :player1)
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
         #(when-not (:ended? (swap! app-state update-game))
            (animate))))

(defn start-game []
  (swap! app-state #(reset-players (new-game (:player1 %) (:player2 %))))
  (animate))

(defn move-right [player]
  (swap! app-state update player
         (fn [player] (-> player
                          (update-in [:velocity 0] #(+ max-speed-x (min 0 %)))
                          (assoc :face :right)))))

(defn move-left [player]
  (swap! app-state update player
         (fn [player] (-> player
                          (update-in [:velocity 0] #(- (max 0 %) max-speed-x))
                          (assoc :face :left)))))

(defn jump [player]
  (swap! app-state update player update-in [:velocity 1] #(if (zero? %) jump-speed-y %)))

(defn init []
  (when-not @app-state
    (println "init app:" @app-state)
    (reset! app-state initial-state)))

(defn change-avatar [player i f]
  (let [max-i (dec (count avatar-names))]
    (swap! player assoc :avatar
           (nth avatar-names
                (swap! i #(if (zero? %) max-i (f %)))))))

(defn home [e]
  (swap! app-state #(->PlayerSelection (:player1 %) (:player2 %)))
  (doto e (ocall "preventDefault") (ocall "stopPropagation")))
