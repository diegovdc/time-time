(ns sequencing-2
  (:require [overtone.music.time :refer [apply-at now]]))

(def config (atom {:lag 1000}))
(def voice (atom {:fn #(println "hola")
                  :started-at 1227874823742348
                  :next-event (+ 1000 (now))
                  :durs [1 1/3 1/3 1/3]
                  :index 0
                  :ratio 1
                  :tempo 2000
                  :loop? true
                  :playing? true}))
(do
  (swap! voice #(merge % {:loop? true :next-event (+ 1000 (now))}))
  (declare schedule)
  (defn update-voice-and-schedule-next-event 
    [voice]
    (let [{:keys [durs index tempo loop?] :as v} @voice
          next-index(inc index)
          finished? (>= next-index (count durs))
          reset? (user/spy (and finished? loop?))
          next-index* (if-not reset? next-index 0)
          next-event (+ (* tempo (durs next-index*)) 
                        (:next-event v))
          schedule? (or loop? (not finished?))
          ]
      (swap! voice #(merge % {:index (user/spy :index next-index*)
                              :next-event next-event}))
      (when schedule? (schedule voice))
        
      ))

  (defn schedule [voice]
    (let [{:keys [next-event fn]} @voice
          fn* 
          #(when (:playing? @voice) 
             (do 
               (fn)
               (update-voice-and-schedule-next-event voice)))
          event #(do
                   (println "scheduleando")          
                   (apply-at next-event fn*))]
      (apply-at (- next-event (:lag @config)) event)))

  (schedule voice))

(swap! voice #(assoc % :loop? false))

