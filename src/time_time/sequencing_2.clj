(ns time-time.sequencing-2
  (:require [overtone.music.time :refer [apply-at now]]))

(def config (atom {:lag 100}))
(def initial-elapsed-at 0)
(def voice (atom {:fn #(println "adios" %)
                  :started-at (+ 1000 initial-elapsed-at (now))
                  :next-event nil
                  :elapsed-at 0
                  :durs [1 1/4 1/4 1/4 1/4]
                  :index 0
                  :ratio 1
                  :tempo 1000
                  :loop? true
                  :playing? true}))

(do
  #_(swap! voice #(merge % {:loop? true :next-event (+ 1000 (now))}))

  (defn calculate-next-voice-state
    "Calculates next state of a voice and if it should be scheduled"
    [voice]
    (let [{:keys [durs index tempo loop? started-at elapsed-at ratio]} voice
          next-index (inc index)
          finished? (>= next-index (count durs))
          reset? (and finished? loop?)
          next-index* (mod next-index (count durs))
          next-dur (nth durs next-index* index)
          elapsed-at* (+ (* ratio next-dur) elapsed-at)
          next-event (+ started-at (* tempo elapsed-at*))
          schedule? (or loop? (not finished?))]
      {:voice (merge voice {:index next-index
                            :next-event next-event
                            :elapsed-at elapsed-at*})
       :schedule? schedule?}))
  
  (defn schedule! [voice]
    (let [{:keys [next-event fn] :as data} @voice
          fn*
          #(when (:playing? @voice)
             (do
               (fn data)
               (let [{schedule? :schedule? voice* :voice} (calculate-next-voice-state
                                                           @voice)]
                 (reset! voice voice*)
                 (when schedule? (schedule! voice)))))
          event #(do
                   
                   (apply-at next-event fn*))]
      (apply-at (- next-event (:lag @config)) event)))

  #_(schedule! voice))

#_(swap! voice #(assoc % :loop? false))
