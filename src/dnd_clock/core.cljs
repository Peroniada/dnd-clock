(ns dnd-clock.core
  (:require
    [reagent.core :as r :refer [atom]]
    [reagent.dom :as rd]
    [cljs-time.core :as time]
    [cljs-time.format :as date-format]
    [cljs.pprint :refer [pprint]]
    [alandipert.storage-atom :refer [local-storage]]
    [cljs.pprint :refer [pprint]]))

(enable-console-print!)

(println "This text is printed from src/dnd-clock/core.cljs. Go ahead and edit it and see reloading in action.")

;; define your app data so that it doesn't get over-written on reload
;; constants
(def initial-time (time/date-time 1016 6 14 4 3 27 456))

(def time-formatter
  (date-format/formatter "dd-MM-yyyy, HH:mm:ss"))

(defn time-to-string [date-time]
  (date-format/unparse time-formatter date-time))

(defn time-from-string [date-in-string]
  (date-format/parse time-formatter date-in-string))

(defn increase-time [date-time how-much]
  (time/plus date-time how-much))

(defn from-60-to-120 []
  (+ 60 (rand-int 61)))

(defn from-8-10 []
  (+ 8 (rand-int 10)))

(def prefs (local-storage (atom {}) :prefs))

;; time modifiers
(def one-second (time/seconds 1))
(def encounter-time-increase-step (time/seconds 6))
(defn short-rest-time-increase [] (time/minutes (from-60-to-120)))
(defn long-rest-time-increase [] (time/hours (from-8-10)))
(defn travel-time-increase [how-many] (time/minutes how-many))

;; functions
(defn increase-time-normal [date-time]
  (time/plus date-time one-second))

(defn increase-time-encounter [date-time]
  (increase-time date-time encounter-time-increase-step))

(defn increase-time-short-rest [date-time]
  (increase-time date-time (short-rest-time-increase)))

(defn increase-time-long-rest [date-time]
  (increase-time date-time (long-rest-time-increase)))

(defn increase-time-travel [date-time increased-time]
  (increase-time date-time (travel-time-increase increased-time)))

;; initializers
(defn state-change-event [new-time last-time new-state old-state]
  (conj {
         :current-time  new-time
         :last-time     (if-not (nil? last-time) last-time initial-time)
         :diff          (if-not (nil? last-time) (time/in-seconds (time/interval last-time new-time)) 0)
         :current-state (name new-state)
         :last-state    (if-not (nil? old-state) (name old-state) "normal")
         }))

;; deserialization
(defn events-to-string [events]
  (mapv
    (fn [event] conj {
                      :current-time  (time-to-string (:current-time event))
                      :last-time     (time-to-string (:last-time event))
                      :diff          (:diff event)
                      :current-state (:current-state event)
                      :last-state    (:last-state event)
                      })
    events))

(defn to-clj-map [my-map]
  (into {}
        (for [[k v] my-map]
          [(keyword k) v])))

(defn events-map-to-clj [events]
  (map
    (fn [event]
      (to-clj-map event))
    events))

(defn events-from-string [events]
  (mapv
    (fn [event] (state-change-event
                  (time-from-string (:current-time event))
                  (time-from-string (:last-time event))
                  (:current-state event)
                  (:last-state event)))
    events))

(defn convert-events-to-json [events]
  (.stringify js/JSON (clj->js (events-to-string events))))

(defn parse-events-json [json]
  (events-from-string (events-map-to-clj (js->clj (.parse js/JSON json)))))

(defn initialize-time []
  (if-not (empty? (:time @prefs)) (time-from-string (:time @prefs)) initial-time))

(defn initialize-time-shifts []
  (if-not (empty? (:events @prefs))
    (parse-events-json (:events @prefs))
    (conj [] (state-change-event initial-time initial-time :normal :normal))))

(defn initialize-game-state []
  (if-not (empty? (:events @prefs))
    (keyword (:current-state (last (parse-events-json (:events @prefs)))))
    :pause))

(def possible-states [:normal :encounter :travel :short-rest :long-rest])
(def game-state (atom (initialize-game-state)))
(def game-time (atom (initialize-time)))
(def time-shifts (atom (initialize-time-shifts)))
(def timeout-id (atom 0))

(add-watch time-shifts
           :time-changed
           (fn [_ _ _ _]
             (swap! prefs assoc :time (time-to-string @game-time))
             (swap! prefs assoc :events (convert-events-to-json @time-shifts))
             ))

(defn last-time-event []
  (last @time-shifts))

(defn last-time-shift []
  (:current-time (last-time-event)))

(defn last-time-state []
  (:current-state (last-time-event)))

(defn timer-component []
  (fn []
    (reset! timeout-id (js/setTimeout #(swap! game-time increase-time-normal game-time) 1000))
    [:h2 (time-to-string @game-time)]))

(defn encounter-component []
  [:h2 (time-to-string @game-time)
   [:input {:type     "button" :value "Next Turn"
            :on-click #(swap! game-time increase-time-encounter game-time)}]])

(defn rest-component [time-increase, label]
  [:h2 (time-to-string @game-time)
   [:input {:type     "button" :value label
            :on-click #(swap! game-time time-increase game-time)}]])

(def speed-map {
                :slow     {:feet-per-min 200 :meters-per-min 60}
                :moderate {:feet-per-min 300 :meters-per-min 90}
                :fast     {:feet-per-min 400 :meters-per-min 120}
                })

(def travel-speed (atom (speed-map :moderate)))
(def tile-size (atom {:tile-size-feet 200 :tile-size-meters 60}))
(defn move-one-tile-time [size speed]
  (int (/ size speed)))

;; -------------------------
;; Page components

(defn slider [param value min max]
  [:input {:type      "range" :value value :min min :max max
           :style     {:width "100%"}
           :on-change (fn [e]
                        (let [new-value (js/parseInt (.. e -target -value))]
                          (swap! tile-size
                                 (fn [data]
                                   (-> data
                                       (assoc param new-value)
                                       )))))}])

(defn travel-component []
  (let [{:keys [tile-size-meters tile-size-feet]} @tile-size
        {:keys [feet-per-min meters-per-min]} @travel-speed]
    [:div
     [:h2 (time-to-string @game-time)]
     [:h3 "Speed in feet: " feet-per-min " feet/min"]
     [:h3 "Speed in meters: " meters-per-min " meters/min"]
     [:div.btn-group {:field :single-select :id :unique.position}
      [:input {:type     "button" :value "Slow"
               :on-click #(reset! travel-speed (speed-map :slow))}]
      [:input {:type     "button" :value "Moderate"
               :on-click #(reset! travel-speed (speed-map :moderate))}]
      [:input {:type     "button" :value "Fast"
               :on-click #(reset! travel-speed (speed-map :fast))}]
      ]
     [:div "Tile size: " tile-size-meters "m | " (Math/ceil (* 3.33333 tile-size-meters)) " feet"]
     [slider :tile-size-meters tile-size-meters 100 10000]
     [:div "Time to move one tile: " (move-one-tile-time tile-size-meters meters-per-min)]
     [:input {:type     "button" :value "Move one tile"
              :on-click #(reset! game-time (increase-time-travel @game-time (move-one-tile-time tile-size-meters meters-per-min)))}]
     ]))

(defn pause-component []
  [:h2 (time-to-string @game-time) " PAUSED"])

(defn get-time-component []
  (case @game-state
    :pause [pause-component]
    :normal [timer-component]
    :encounter [encounter-component]
    :short-rest [rest-component increase-time-short-rest "Short Rest"]
    :long-rest [rest-component increase-time-long-rest "Long Rest"]
    :travel [travel-component]
    ))

(defn last-phase-info [shift]
  (str "Last Phase: " (:last-state shift) " took: " (:diff shift) " seconds. "))

(defn current-phase-info [shift]
  (str (:current-state shift) " started at: " (time-to-string (:current-time shift))))

(defn time-shifts-component []
  [:div
   [:div "Time shifts: "]
   [:ul (if-not (empty? @time-shifts)
          (map (fn [shift]
                 [:li {:key (str (random-uuid)) :style {:border-style "solid"}}
                  [:div (last-phase-info shift)]
                  [:div (current-phase-info shift)]]) (reverse @time-shifts)))]
   ])

(defn interval-in-seconds-between [date-time other]
  (time/in-seconds (time/interval date-time other))
  )


(defn change-state! [next-state]
  ;(if-not (= @game-state next-state)
    #(reset! game-state next-state)
    ;#()
    ;)
    )

(defn update-events! [next-state]
  (if-not (or
            (zero? (interval-in-seconds-between (last-time-shift) @game-time))
            (= @game-state next-state))
    (do
      ;(reset! game-state next-state)
      (swap! time-shifts conj (state-change-event @game-time (last-time-shift) next-state (last-time-state))))
    #())
  )

(defn pause-toggle! []

  (if (= @game-state :pause)
    #(reset! game-state (keyword (last-time-state)))
    #(reset! game-state :pause)
    )
  )

(defn choose-state-component []
  [:div
   [:div @game-state]
   [:div
    [:input {:type     "button" :value "Pause"
             :on-click #(
                         (pause-toggle!)
                         )}]
    [:input {:type     "button" :value "Normal"
             :on-click #(
                         (change-state! :normal)
                         (update-events! :normal)
                         )}]
    [:input {:type     "button" :value "Encounter"
             :on-click #(
                         (change-state! :encounter)
                         (update-events! :encounter)
                         (js/clearTimeout @timeout-id)
                         )}]
    [:input {:type     "button" :value "Travel"
             :on-click #(
                         (change-state! :travel)
                         (update-events! :travel)
                         (js/clearTimeout @timeout-id)
                         )}]
    [:input {:type     "button" :value "Short Rest"
             :on-click #(
                         (change-state! :short-rest)
                         (update-events! :short-rest)
                         (js/clearTimeout @timeout-id)
                         )}]
    [:input {:type     "button" :value "Long Rest"
             :on-click #(
                         (change-state! :long-rest)
                         (update-events! :long-rest)
                         (js/clearTimeout @timeout-id)
                         )}]]
   [get-time-component]
   ])
(defn hello-world []
  [:div {:class-name "body-container"}
   [:h1 "Welcome to dnd-clock"]
   [choose-state-component]
   [time-shifts-component]])

(rd/render [hello-world]
           (. js/document (getElementById "app")))

(defn on-js-reload []
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
  )
