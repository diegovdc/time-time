(ns time-time.sequencing-2
  (:require [overtone.music.time :refer [apply-at now]]))

(def config (atom {:lag 100}))

(do

  (defn calculate-next-voice-state
    "Calculates next state of a voice and if it should be scheduled"
    [voice]
    (let [{:keys [durs index tempo loop? started-at elapsed-at ratio]} voice
          next-index (inc index)
          finished? (>= next-index (count durs))
          ;; reset? (and finished? loop?)
          next-index* (mod next-index (count durs))
          next-dur (nth durs next-index* index)
          elapsed-at* (+ (* ratio next-dur) elapsed-at)
          next-event (+ started-at (* tempo elapsed-at*));; TODO tempo should be something other than milliseconds
          schedule? (or loop? (not finished?))]
      {:voice (merge voice {:index next-index
                            :next-event next-event
                            :elapsed-at elapsed-at*})
       :schedule? schedule?}))

  (defn schedule! [voice]
    (let [{:keys [next-event f] :as data} @voice
          fn* #(when (:playing? @voice)
                 (do
                   (f {:data data :voice voice})
                   (let [{schedule? :schedule? voice* :voice}
                         (calculate-next-voice-state @voice)]
                     (reset! voice voice*)
                     (when schedule? (schedule! voice)))))
          event #(apply-at next-event fn*)]
      (apply-at (- next-event (:lag @config)) event))))

(comment
  (do ;; Setup
    (def initial-elapsed-at 0)
    (def voice (atom {:f (fn [{:keys [data voice]}]
                            (println "data" data)
                            (swap! voice assoc :durs [(rand-nth [1/4 1])]))
                      :started-at (+ 1000 initial-elapsed-at (now))
                      :next-event nil
                      :elapsed-at 0
                      :durs [1 1/4 1/4 1/4 1/4]
                      :index 0
                      :ratio 1
                      :tempo 1000
                      :loop? true
                      :playing? true})))
  (do ;; Test
    (swap! voice #(merge % {:loop? true
                            :elapsed-at 0
                            :started-at (+ 1000 initial-elapsed-at (now))
                            :next-event (+ 1000 (now))
                            :playing? true}))
    (schedule! voice))
  ;;; Stop
  (swap! voice #(assoc % :loop? false :playing? false)))
