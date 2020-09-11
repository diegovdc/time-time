(ns time-time.sequencing-3
  "`play!` is the main function of this ns, `schedule!` is a lower level function, that might be helpful.
  `:elapsed` is the amount of time elapsed by the end of the `:current-event`
    and it's measured in `milliseconds`"
  (:require
   [time-time.standard :refer [dur->ms wrap-at]]
   [overtone.music.time :refer [apply-at now]]
   [taoensso.timbre :as log]))


(declare schedule!)
(defn play!
  "Gets a config and returns a `voice-atom` that will start playing,
  if `playing?` is true (default)"
  [durs on-event &
   {:keys [ratio tempo loop? start-index start-time elapsed playing?
           before-update on-schedule extra-data]
    :or {ratio 1
         tempo 60
         loop? false
         start-index 0
         start-time (now)
         elapsed 0
         playing? true
         before-update identity ;; receives the voice data before it is used to `reset!` the `voice-atom`
         on-schedule (fn [voice event-schedule] event-schedule)
         extra-data {}}}]
  (let [voice (atom (merge extra-data
                           {:durs durs
                            :on-event on-event
                            :ratio ratio
                            :tempo tempo
                            :loop? loop?
                            :index start-index
                            :started-at start-time
                            ;; maybe FIXME, perhaps `play!` should receive
                            ;; `elapsed-ms` instead of `elapsed`
                            :elapsed-ms (dur->ms elapsed tempo)
                            :playing? playing?
                            :before-update before-update
                            :on-schedule on-schedule}))]
    (schedule! voice)
    voice))

(defn calculate-next-voice-state
  [{:keys [index durs elapsed-ms ratio tempo] :as voice}]
  (let [dur (-> (wrap-at index durs) (* ratio))
        event-dur (dur->ms dur tempo)
        updated-state {:index (inc index)
                       :elapsed-ms (+ elapsed-ms event-dur)
                       :current-event {:dur-ms event-dur :dur dur}}]
    (merge voice updated-state)))

(defn play-event?
  "Based on the index, determine if a voice has an event that should be
  played."
  [index durs loop?]
  (or (< index (count durs)) loop?))


(defn schedule?
  "Based on the index, determine if a voice has an event that should be
  scheduled."
  [index durs loop?]
  (play-event? index durs loop?))

(defn update-voice [before-update voice-update data]
  (-> data
      (merge
       ;; TODO calculate-next-voice-state should only return the fields below
       (select-keys voice-update
                    [:index
                     :elapsed-ms
                     :current-event]))
      before-update))

(defn schedule! [voice-atom]
  (let [{:keys [started-at elapsed-ms] :as v} @voice-atom
        event-schedule (log/spy (+ started-at elapsed-ms))
        next-voice-state (calculate-next-voice-state v)
        on-event* (fn []
                    (let [v* @voice-atom
                          {:keys [on-event before-update index durs loop?]
                           :or {before-update identity}} v]
                      (when (:playing? v*)
                        (log/debug "About to play event")
                        (try (do
                               (when (play-event? index durs loop?)
                                 ;; TODO current-event should always be present
                                 (on-event {:data v* :voice voice-atom}))
                               (swap! voice-atom (partial update-voice
                                                          before-update
                                                          next-voice-state))
                               (when (schedule? (next-voice-state :index)
                                                (next-voice-state :durs)
                                                loop?)
                                 (schedule! voice-atom)))
                             (catch Exception e (log/error e))))))]
    (apply-at event-schedule on-event*)))

(comment
  (apply-at (+ 1000 (now)) println "hola"))

(comment
  (def voice-state {:durs [1]
                    :index 0
                    :tempo 60
                    :loop? true
                    :started-at 0
                    :elapsed 0
                    :ratio 1})

  (def v (atom (assoc voice-state :on-event
                      (fn [{:keys [data voice]}]
                        (println data)))))

  (do
    (swap! v assoc :started-at (+ 1000 (now)) :playing? true)
    (schedule! v))

  (swap! v assoc :playing? false)

  (def v1 (play! [1] (fn [_] (println "hola")
                       (swap! v1 assoc :durs [(inc (rand 3))]))
                 :loop? true
                 :ratio 1/4
                 :start-time (+ 2000 (now))))
  (swap! v1 assoc :playing? false)

  (swap! v1 assoc :on-event
         (fn [_] (println "holassssss")
           (println (:durs (swap! v1 assoc :durs [(inc (rand 3))]))))))
