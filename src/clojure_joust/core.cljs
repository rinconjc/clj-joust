(ns ^:figwheel-hooks clojure-joust.core
  (:require
   [goog.dom :as gdom]
   [reagent.core :as reagent :refer [atom]]))

(println "This text is printed from src/clojure_joust/core.cljs. Go ahead and edit it and see reloading in action.")

(defn multiply [a b] (* a b))


;; define your app data so that it doesn't get over-written on reload
(defonce app-state (atom {:text "Clojure Joust!"}))

(defn get-app-element []
  (gdom/getElement "app"))

(defn hello-world []
  [:div.container
   [:h4 (:text @app-state)
    [:img {:src "icon.png"}]]
   [:div.row
    [:div.col.s4.white
     "test"]
    [:div.col.s4]
    [:div.col.s4.white]]
   ;; [:svg.main {:width "100%" :height 400
   ;;             :fill "white"}
   ;;  [:defs
   ;;   [:pattern {:id "bgimg" :pattern-units "userSpaceOnUse" :x 0:y 0
   ;;              :width "100%" :height "100%"}
   ;;    [:image {:xlinkHref "/images/waterfall.png" :preserve-aspect-ratio "none"
   ;;             :width "100%" :height "100%"} ]]]
   ;;  [:rect {:width "100%" :height "100%" :fill "url(#bgimg)"}]
   ;;  ]
   ;; [:div.character-pick.left]
   ;; [:div.character-pick.right]
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
