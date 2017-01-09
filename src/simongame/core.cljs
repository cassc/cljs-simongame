(ns simongame.core
  (:require
   [clojure.string :as s]
   [alandipert.storage-atom :refer [local-storage]]
   [reagent.core :as reagent :refer [atom]]
   [cljs.core.async :refer [<! chan sliding-buffer put! close! timeout alts!]])
  (:require-macros
   [cljs.core.async.macros :refer [go-loop go]]))

(enable-console-print!)

(def success-count 20)
(def timeout-val 5000)
(defonce finished-colors (atom []))
(defonce pending-colors (atom []))
(defonce staging-colors (atom []))
(defonce play-state (atom {:start false :strict false :state :off :current nil}))
(defonce active-color (atom {}))
(defonce tos (chan (sliding-buffer 10)))

(defn play [key]
  (letfn [(play-audio [f] (.play (js/Audio. f)))]
    (case key
      :red    (play-audio "simonSound1.mp3")
      :green  (play-audio "simonSound2.mp3")
      :yellow (play-audio "simonSound3.mp3")
      :blue   (play-audio "simonSound4.mp3")
      ;; error sound taken from
      ;; http://soundbible.com/tags-error.html
      :error (play-audio "error.mp3"))))

(defn reset-game []
  (let [state (:state @play-state)]
    (put! tos :mon-off)
    (swap! play-state
           assoc :state (case state :on :off :off :on) :start nil :strict nil :win false)))

(defn rand-color []
  (rand-nth [:red :green :yellow :blue]))

(defn async-do-seq
  ([xs play-time pause-time]
   (swap! play-state assoc :clickable false)
   (go-loop [xs xs]
     (if-let [color (first xs)]
       (do
         (case color
           :pause (<! (timeout 1000))
           :mon-on (>! tos color)
           :mon-off (>! tos color)
           
           (do
             (reset! active-color {color true})
             (play color)
             (<! (timeout play-time))
             (reset! active-color {})
             (<! (timeout pause-time))))
         (recur (rest xs)))
       (swap! play-state assoc :clickable true))))
  ([xs]
   (async-do-seq xs 1000 200)))

(defn start-game!
  ([]
   (start-game! nil nil))
  ([pre-play post-play]
   (let [c (rand-color)]
     (reset! finished-colors [])
     (async-do-seq (filter identity [:mon-off pre-play c post-play :mon-on]))
     (swap! play-state assoc :start true :win false)
     (reset! pending-colors [c])
     (reset! staging-colors []))))

(defn notify-win! []
  (put! tos :mon-off)
  (async-do-seq [:pause :green :red :blue :yellow] 150 10)
  (swap! play-state assoc :win true :start false :strict false))

(defn notify-error! []
  (put! tos :mon-off)
  (if-not (:strict @play-state)
    (do
      (reset! pending-colors (vec (concat @staging-colors @pending-colors)))
      (reset! staging-colors [])
      (async-do-seq (concat [:error] @pending-colors [:mon-on])))
    (start-game! :error :pause)))

(defn timeout-check []
  (let [mon (atom nil)]
    (go-loop []
      (let [[v ch] (alts! [tos (timeout timeout-val)])]
        (println (js/Date.) v)
        (if-not v
          (when (and @mon (:start @play-state))
            (notify-error!))
          (case v
            :mon-on (reset! mon true)
            :mon-off (reset! mon false)
            nil))
        (recur)))))

(defn click [color]
  (fn []
    (when-let [c (and (:start @play-state)
                      (:clickable @play-state)
                      (first @pending-colors))]
      (put! tos color)
      (if (= c color)
        (do
          (play color)
          (swap! pending-colors rest)
          (swap! staging-colors conj color)
          (when-not (seq @pending-colors)
            (reset! finished-colors @staging-colors)
            (reset! staging-colors [])
            (if (> (count @finished-colors) success-count)
              (notify-win!)
              (do
                (reset! pending-colors (conj @finished-colors (rand-color)))
                (async-do-seq (vec (concat [:mon-off :pause] @pending-colors [:mon-on])))))))
        (notify-error!)))))

(defn color-button []
  (fn [location color]
    [:div.push {:on-click (click color)
                :class (str location
                            " "
                            (when (color @active-color) "active")
                            " "
                            (when-not (:start @play-state)
                              "unclickable"))}]))

(defn my-app []
  (fn []
    [:div
     [:div.top
      [color-button "top-left" :green]
      [color-button "top-right" :red]]
     [:div.bottom
      [color-button "bottom-left" :yellow]
      [color-button "bottom-right" :blue]]
     [:div.center
      [:div.circle
       [:h3.brand "Simon"
        [:span.registered "®"]]
       [:div.row
        [:div.count
         (let [cnt (cond
                     (:start @play-state) (let [c (inc (count @finished-colors))]
                                            (if (< c 10) (str "0" c) c))
                     (:win @play-state) "**"
                     :else "--")]
           [:div.count-btn {:class (case (:state @play-state) :on "on" :off "off")} cnt])
         "COUNT"]
        [:div.start
         [:div.start-btn {:class (if (:start @play-state) "on" "off")
                          :on-click #(when (and (= :on (:state @play-state))
                                                (not (:start @play-state)))
                                       (start-game!))}]
         "START"]
        [:div.strict
         [:div.strict-led {:class (if (:strict @play-state) "on" "off")}]
         [:div.strict-btn {:on-click #(when (and (= :on (:state @play-state))
                                                 (not (:start @play-state)))
                                        (swap! play-state update-in [:strict] not))}]
         "STRICT"]]
       [:div.switch-slot
        [:div.switch-label "OFF"]
        [:div.switch-btn {:on-click reset-game}
         [:div {:class (case (:state @play-state) :on "on" :off "off")}]]
        [:div.switch-label "ON"]]]]]))

(defn main []
  (timeout-check)
  (reagent/render [#'my-app] (.getElementById js/document "app")))

(main)

