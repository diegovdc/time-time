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
   {:keys [ratio tempo loop? start-index start-time elapsed playing?]
    :or {ratio 1
         tempo 60
         loop? false
         start-index 0
         start-time (now)
         elapsed 0
         playing? true}}]
  (let [voice (atom {:durs durs
                     :on-event on-event
                     :ratio ratio
                     :tempo tempo
                     :loop? loop?
                     :index start-index
                     :started-at start-time
                     :elapsed (dur->ms elapsed tempo)
                     :playing? playing?})]
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
  [{:keys [index durs loop?]}]
  (or (< index (count durs)) loop?))


(defn schedule?
  "Based on the index, determine if a voice has an event that should be
  scheduled."
  [voice]
  (play-event? voice))

(defn schedule! [voice-atom]
  (let [{:keys [started-at elapsed] :as v} @voice-atom
        event-schedule (+ started-at elapsed)
        {:keys [on-event] :as voice-update} (calculate-next-voice-state v)
        on-event* (fn []
                    (let [v* @voice-atom]
                      (when (:playing? v*)
                        (try (do
                               (reset! voice-atom voice-update) ;; NOTE above the on-event function in case the atom is updated by that function... this might be less performant, tho... maybe create an after-event function for updates?
                               (when (play-event? v*)
                                 (on-event {:data v*
                                            :voice voice-atom}))
                               (when (schedule? voice-update)
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
                      :ratio 1/2
                      :on-event (fn [{:keys [data voice]}] (println data)))))

  (do
    (swap! v assoc :started-at (+ 1000 (now)) :playing? true)
    (schedule! v))

  (swap! v assoc :playing? false)

  (def v1 (play! [1 2] (fn [_] (println "hola"))
                 :ratio 1/4
                 :start-time (+ 2000 (now))))
  (swap! v1 assoc :playing? false))
