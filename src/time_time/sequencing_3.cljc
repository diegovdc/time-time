(ns time-time.sequencing-3
  "`play!` is the main function of this ns, `schedule!` is a lower level function, that might be helpful.
  `:elapsed` is the amount of time elapsed by the end of the `:current-event`
    and it's measured in `milliseconds`"
  (:require
   [time-time.standard :refer [dur->ms wrap-at now]]
   #?(:clj
      [overtone.music.time :refer [apply-at stop-player]]
      :cljs ["tone/build/esm/index" :as Tone])
   #?(:clj [taoensso.timbre :as timbre]
      :cljs [taoensso.timbre :as timbre :include-macros true])))

(declare schedule! get-current-dur-data)
(defn play!
  "Gets a config and returns a `voice-atom` that will start playing,
  if `playing?` is true (default)"
  [durs on-event &
   {:keys [ratio tempo loop? start-index start-time elapsed elapsed-dur playing?
           before-update on-schedule extra-data on-startup-error durs-len]
    :or {ratio 1
         tempo 60
         loop? false
         start-index 0
         start-time (now)
         elapsed 0
         elapsed-dur 0 ;; TODO investigate if this is compatible with `elapsed`, if so, substitute
         playing? true
         before-update identity ;; receives the voice data before it is used to `reset!` the `voice-atom`
         on-schedule (fn [voice event-schedule] event-schedule)
         extra-data {}}}]
  (let [voice (atom (merge extra-data
                           {:durs durs
                            :elapsed-dur elapsed-dur
                            :cycle-len (cond
                                         (and (fn? durs) (not durs-len)) (do (timbre/warn "`durs` is a function but no `durs-len` has been provided, defaulting to 1")
                                                                             1)
                                         durs-len durs-len
                                         :else (apply + durs))
                            :on-event on-event
                            :ratio ratio
                            :tempo tempo
                            :loop? loop?
                            :index start-index
                            :started-at start-time
                            :current-event (get-current-dur-data {:tempo tempo
                                                                  :ratio ratio
                                                                  :durs durs
                                                                  :index start-index})
                            ;; maybe FIXME, perhaps `play!` should receive
                            ;; `elapsed-ms` instead of `elapsed`
                            :elapsed-ms (dur->ms elapsed tempo)
                            :playing? playing?
                            :before-update before-update
                            :on-schedule on-schedule
                            :on-startup-error on-startup-error}))]
    (schedule! voice)
    voice))

(defn get-current-dur-data-multi-pred
  [{:as voice :keys [durs]}]
  (cond
    (sequential? durs) :durs-vector
    (fn? durs) :durs-gen-fn
    :else (throw (ex-info "Unknown dur-data, cannot `get-current-dur-data-multi-pred`" voice))))

(defmulti get-current-dur-data
  #'get-current-dur-data-multi-pred)

(defmethod get-current-dur-data :durs-vector
  [{:keys [tempo ratio durs index] :as voice}]
  (let [dur (-> (wrap-at index durs) (* ratio))
        event-dur (dur->ms dur tempo)]
    {:dur dur :event-dur event-dur}))

(defmethod get-current-dur-data :durs-gen-fn
  [{:keys [durs tempo] :as voice}]
  (let [dur (durs voice)
        event-dur (dur->ms dur tempo)]
    {:dur dur :event-dur event-dur}))

(defn calculate-next-voice-state
  [{:keys [index elapsed-ms cycle-len elapsed-dur] :as voice}]
  (let [{:keys [dur event-dur]} (get-current-dur-data voice)
        updated-state {:index (inc index)
                       :elapsed-dur (+ elapsed-dur dur)
                       :elapsed-ms (+ elapsed-ms event-dur)
                       :current-event {:dur-ms event-dur
                                       :dur dur
                                       :index index
                                       :cycle (quot elapsed-dur cycle-len)}}]
    (merge voice updated-state)))

(defn play-event?
  "Based on the index, determine if a voice has an event that should be
  played."
  [index durs loop?]
  (cond
    (sequential? durs) (or (< index (count durs)) loop?)
    (fn? durs) loop?
    :else (throw (ex-info "Cannot play event. `durs` must be vector, a list or a function"
                          {:durs durs}))))

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
                     :current-event
                     :prev-on-event
                     :tempo
                     :durs
                     :loop?
                     :ratio
                     :on-event
                     ;; TODO should refrain/config be updated?
                     :refrain/config]))
      (assoc :previous-state
             (select-keys data
                          [:index
                           :elapsed-ms
                           :current-event]))
      (cond-> (:updated? voice-update)
        (dissoc :update :updated?))
      before-update))

#?(:cljs
   ;; Adds apply-at to shim overtone.music.time/apply-at
   (do
     (def transport (Tone/Transport.start))
     (defn apply-at [time on-event-fn]
       (let [time (/ time 1000)]
         (.scheduleOnce transport on-event-fn time)))))

(defn update-voice-config
  [voice]
  (let [modified-voice (merge voice
                              (:previous-state voice)
                              (:update voice))]
    (assoc (update-voice identity
                         (calculate-next-voice-state modified-voice)
                         modified-voice)
           :updated? true)))

(defn maybe-update-voice [voice]
  (if (:update voice)
    (update-voice-config voice)
    voice))

(defn schedule! [voice-atom]
  (let [v @voice-atom
        v (maybe-update-voice v)
        {:keys [started-at elapsed-ms]} v
        event-schedule (+ started-at elapsed-ms)
        next-voice-state (calculate-next-voice-state v)
        on-event* (fn []
                    (let [v* @voice-atom
                          {:keys [on-event
                                  before-update
                                  index
                                  durs
                                  loop?
                                  tempo
                                  ratio]
                           :or {before-update identity}} v ;; Can we use `v*` here?
                          after-event #(do
                                         (swap! voice-atom (partial
                                                            update-voice
                                                            before-update
                                                            next-voice-state))
                                         (if (schedule? (next-voice-state :index)
                                                        (next-voice-state :durs)
                                                        loop?)
                                           (schedule! voice-atom)
                                           (swap! voice-atom assoc :playing? false)))
                          ;; NOTE assuming an on-event error, else playback
                          ;; schedule! will stop recurring
                          on-error (fn [error]
                                     (timbre/error error)
                                     (cond
                                       (:prev-on-event v*)
                                       (try
                                         ((:prev-on-event v*)
                                          {:data v*
                                           :voice voice-atom
                                           :dur (-> v* :current-event :dur)})
                                         (swap! voice-atom assoc
                                                :on-event
                                                (:prev-on-event v*))
                                         #?(:clj (catch Exception _ (timbre/error "Can not recover from error"))
                                            :cljs (catch js/Error _ (timbre/error "Can not recover from error")))
                                         (finally (after-event)))

                                       (zero? (:index v*))
                                       ((:on-startup-error v*))

                                       ;; probably when there is no `:prev-on-event` and `:index` is not zero.
                                       :else (after-event)))]
                      (when (:playing? v*)
                        #_(timbre/debug "About to play event")
                        (try (when (play-event? index durs loop?)
                               (let [{:keys [dur event-dur]}
                                     (get-current-dur-data v*)]

                                 (on-event {:data (assoc v*
                                                         :dur dur
                                                         :dur-s (/ event-dur 1000)
                                                         :dur-ms event-dur)
                                            :voice voice-atom})))
                             (after-event)
                             #?(:clj (catch Exception e (on-error e))
                                :cljs (catch js/Error e (on-error e)))))))]
    (apply-at event-schedule on-event*)))

(comment
  (play! [1 2 3 1]
         #(-> % :dur-ms println)
         :tempo 120
         :loop? false))

(comment
  (timbre/set-level! :error)
  (def job (apply-at (+ 5000 (now)) #(println "hola")))
  (stop-player job))

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

  (def v1 (play! [1 1 1 2]
                 (fn [_] (println "hola"))
                 :loop? false
                 :ratio (/ 1 4)
                 :start-time (+ 2000 (now))))
  (swap! v1 assoc :playing? false)

  (swap! v1 assoc :on-event
         (fn [_] (println "holassssss")
           (println (:durs (swap! v1 assoc :durs [(inc (rand 3))]))))))

(comment
  (def synth (.toDestination (Tone/Synth.)))

  (def js-version
    ;; NOTE now is in seconds not ms
    (play! [1 1 1 2]
           (fn [_] (.triggerAttackRelease synth "C4" "8n"))
           :loop? false
           :ratio (/ 1)
           :start-time (+ 1 (now)))))
