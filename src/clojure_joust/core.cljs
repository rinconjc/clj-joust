(ns ^:figwheel-hooks clojure-joust.core
  (:require
   [goog.dom :as gdom]
   [reagent.core :as r :refer [atom]]
   [oops.core :refer [oget ocall oset!]]))

(println "This text is printed from src/clojure_joust/core.cljs. Go ahead and edit it and see reloading in action.")

(defn multiply [a b] (* a b))


;; define your app data so that it doesn't get over-written on reload
(defonce app-state (atom {:text "Clojure Joust!"
                          :player1 {:avatar :lion}
                          :player2 {:avatar :bear}}))

(def avatars {:lion "https://blog.udemy.com/wp-content/uploads/2014/04/shutterstock_15856930.jpg"
              :bear "https://www.exploringnature.org/wordpress/wp-content/uploads/2015/01/bear_standing_up.jpg"
              :bandicoot "http://pre08.deviantart.net/0088/th/pre/i/2016/092/2/f/crash_bandicoot_smashified__by_sean_the_artist-d9xfq0i.png"
              :lycanroc "https://orig00.deviantart.net/68dc/f/2017/219/e/a/lycanroc_midnight_dusk_form_2_by_tarun2207-dbj6fgh.png"})

(defn get-app-element []
  (gdom/getElement "app"))

(defn player [name avatar]
  [:div
   [:img.character-pick {:src (avatars avatar)}]
   [:a.left [:i.material-icons "chevron_left"]]
   [:a.right [:i.material-icons "chevron_right"]]
   [:span name]])

(defn player-selection []
  [:div.row
   [:div.col.s5.m3.offset-m2.white
    [player "player 1" :bandicoot]]
   [:div.col.s2.center.middle.full-h "VS"]
   [:div.col.s5.m3.white
    [player "player 2" :lycanroc]]
   [:div.col.s12
    [:div.center [:a.btn {:on-click #(swap! app-state assoc :page :arena)} "Start"]]]])

(defn game-arena []
  (r/create-class
   {:reagent-render
    (fn []
      [:div.box.center
       [:h2 "Jousting Arena"]
       [:div.arena.content
        [:canvas {:id "arena" :ref "arena"}]
        [:div.platform]]
       ])
    :component-did-mount
    (fn[c]
      (let [ctx (-> c (oget "refs") (oget :arena) (ocall "getContext" "2d"))]
        (oset! ctx "fillStyle" "green")
        (ocall ctx "fillRect" 10 10 150 150)
        )
      )}
   ))

(defn setup [])

(def pages {:players #'player-selection
            :arena #'game-arena})

(defn main-page []
  [:div.box
   [:h4 (:text @app-state)
    [:img {:src "icon.png"}]]
   [(-> @app-state :page pages)]
   ])

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
