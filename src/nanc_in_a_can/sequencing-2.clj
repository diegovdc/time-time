(ns nanc-in-a-can.sequencing-2
  (:require [overtone.music.time :refer [apply-at now]]))

(def config (atom {:lag 100}))
(def voice (atom {:fn #(println "adios")
                  :started-at 1227874823742348
                  :next-event (+ 1000 (now))
                  :durs [1 1/4 1/4 1/4 1/4]
                  :index 0
                  :ratio 1
                  :tempo 2000
                  :loop? true
                  :playing? true}))
(do
  (swap! voice #(merge % {:loop? true :next-event (+ 1000 (now))}))

  (defn calculate-next-voice-state
    "Calculates next state of a voice and if it should be scheduled"
    [voice]
    (let [{:keys [durs index tempo loop?]} voice
          next-index (inc index)
          finished? (>= next-index (count durs))
          reset? (and finished? loop?)
          next-index* (if-not reset? next-index 0)
          next-event (+ (* tempo (nth durs next-index* index))
                        (:next-event voice))
          schedule? (or loop? (not finished?))]
      {:voice (merge voice {:index next-index*
                            :next-event next-event})
       :schedule? schedule?}))
  
  (defn schedule [voice]
    (let [{:keys [next-event fn]} @voice
          fn*
          #(when (:playing? @voice)
             (do
               (fn)
               (let [{schedule? :schedule? voice* :voice} (calculate-next-voice-state
                                                           @voice)]
                 (reset! voice voice*)
                 (when schedule? (schedule voice)))))
          event #(do
                   (println "scheduleando")
                   (apply-at next-event fn*))]
      (apply-at (- next-event (:lag @config)) event)))

  (schedule voice))

(swap! voice #(assoc % :loop? false))
