(ns time-time.sequencing-3
  "`play!` is the main function of this ns, `schedule!` is a lower level function, that might be helpful.
  `:elapsed` is the amount of time elapsed by the end of the `:current-event`"
  (:require
   [time-time.standard :refer [dur->ms wrap-at]]
   [overtone.music.time :refer [apply-at now]]))


(declare schedule!)
(defn play!
  "Gets a config and returns a `voice-atom` that will start playing,
  if `playing?` is true (default)"
  [durs on-event &
   {:keys [ratio tempo loop? start-index start-time elapsed playing?
           before-update extra-data]
    :or {ratio 1
         tempo 60
         loop? false
         start-index 0
         start-time (now)
         elapsed 0
         playing? true
         before-update identity ;; receives the voice data before it is used to `reset!` the `voice-atom`
         extra-data {}}}]
  (let [voice (atom (merge extra-data
                           {:durs durs
                            :on-event on-event
                            :ratio ratio
                            :tempo tempo
                            :loop? loop?
                            :index start-index
                            :started-at start-time
                            :elapsed (dur->ms elapsed tempo)
                            :playing? playing?
                            :before-update before-update}))]
    (schedule! voice)
    voice))

(defn calculate-next-voice-state
  [{:keys [index durs elapsed ratio tempo] :as voice}]
  (let [dur (-> (wrap-at index durs) (* ratio))
        event-dur (dur->ms dur tempo)
        updated-state {:index (inc index)
                       :elapsed (+ elapsed event-dur)
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

(defn schedule! [voice-atom]
  (let [{:keys [started-at elapsed] :as v} @voice-atom
        event-schedule (+ started-at elapsed)
        voice-update (calculate-next-voice-state v)
        on-event* (fn []
                    (let [v* @voice-atom
                          {:keys [on-event before-update
                                  index durs loop?] ;; TODO move on
                           :or {before-update identity}} v]
                      (when (:playing? v*)
                        (try (do
                               (when (play-event? index durs loop?)
                                 (on-event {:data v* :voice voice-atom}))
                               (swap! voice-atom
                                      (fn [data]
                                        (-> data
                                            (merge
                                             ;; TODO calculate-next-voice-state should only return the fields below
                                             (select-keys voice-update
                                                          [:index
                                                           :elapsed
                                                           :current-event]))
                                            before-update)))
                               (when (schedule? (voice-update :index)
                                                (voice-update :durs)
                                                loop?)
                                 (schedule! voice-atom)))
                             (catch Exception e (println e))))))]
    (apply-at event-schedule on-event*)))


(comment
  (def voice-state {:durs [1 2 1]
                    :index 0
                    :tempo 60
                    :loop? false
                    :started-at 0
                    :elapsed 0
                    :ratio 1})
  (def v (atom (assoc voice-state
                      :on-event (fn [{:keys [data voice]}] (println data)))))

  (do
    (swap! v assoc :started-at (+ 1000 (now)) :playing? true)
    (schedule! v))

  (swap! v assoc :playing? false)

  (def v1 (play! [1 2] (fn [_] (println "hola"))
                 :ratio 1/4
                 :start-time (+ 2000 (now))))
  (swap! v1 assoc :playing? false))
