(ns time-time.sequencing-4
  (:require
   [overtone.music.time :refer [apply-at now]]
   [taoensso.timbre :as timbre]
   [time-time.standard :refer [dur->ms wrap-at]]))

(declare get-current-dur-data calculate-cycle-delta play-event? get-cycle-len get-cycle-data calculate-next-voice-state)

(defn init-voice-data
  [{:as config
    :keys [durs on-event ratio tempo loop? start-index start-time elapsed elapsed-dur playing?
           before-update on-schedule extra-data on-startup-error _cycle-len]
    :or {durs [1]
         on-event (fn [{:keys [_event _voice]}])
         ratio 1
         tempo 60
         loop? false
         start-index 0
         start-time (now)
         elapsed 0
         elapsed-dur 0 ;; TODO investigate if this is compatible with `elapsed`, if so, substitute
         playing? true
         before-update identity ;; receives the voice data before it is used to `reset!` the `voice-atom`
         on-schedule (fn [_voice event-schedule] event-schedule)
         extra-data {}}}]
  (let [cycle-data (get-cycle-data (assoc config :ratio ratio))
        data (merge
              extra-data
              {:durs durs
               :on-event on-event
               :elapsed-dur elapsed-dur
               :ratio ratio
               :tempo tempo
               :loop? loop?
               :index start-index
               :cycle-0-index start-index
               :new-cycle? true
               :started-at start-time
               ;; maybe FIXME, perhaps `play!` should receive
               ;; `elapsed-ms` instead of `elapsed`
               :elapsed-ms (dur->ms elapsed tempo)
               :playing? playing?
               :before-update before-update
               :on-schedule on-schedule
               :on-startup-error on-startup-error}
              cycle-data)
        data (calculate-next-voice-state data)]
    (atom data)))

(defn calculate-next-voice-state
  [{:keys [index
           elapsed-ms
           cycle-len
           cycle-0-index
           cycle
           new-cycle?
           elapsed-dur
           cycle-delta
           started-at
           playing?
           durs
           loop?] :as voice}]
  (let [{:keys [dur event-dur]} (get-current-dur-data voice)
        elapsed-dur*  (+ elapsed-dur dur)
        index* (inc index)
        cycle* (quot elapsed-dur* cycle-len)
        cycle-delta* (calculate-cycle-delta elapsed-dur* cycle-len)
        updated-state {:index index*
                       :cycle cycle*
                       :cycle-delta cycle-delta*
                       :cycle-0-index (if (zero? cycle-delta*) index* cycle-0-index)
                       :new-cycle? (not= cycle cycle*)
                       :elapsed-dur elapsed-dur*
                       :elapsed-ms (+ elapsed-ms event-dur)
                       :playing? (and playing? (play-event? index durs loop?))
                       :current-event {:index index
                                       :dur-ms event-dur
                                       :dur-s (/ event-dur 1000)
                                       :dur dur
                                       :durs durs
                                       :cycle cycle
                                       :new-cycle? new-cycle?
                                       :cycle-len cycle-len
                                       :cycle-delta cycle-delta
                                       :cycle-0-index cycle-0-index
                                       :cycle-delta-index (- index cycle-0-index)
                                       :elapsed-ms elapsed-ms
                                       :elapsed-dur elapsed-dur
                                       :start-at (+ started-at elapsed-ms)}}]
    (merge voice updated-state)))

(comment
  (init-voice-data {:durs [1 2 3]
                    ;; :on-event println
                    ;; :ratio 1
                    })
  (def v1 (init-voice-data {:durs [1 2 3]
                            :on-event println
                            :ratio 1}))

  (update-voice-state! v1)

  (swap! v1 assoc
         ;; :update? (fn [data] (zero? (:cycle-delta data)))
         :update {:reset-cycle? true
                  :index 0
                  :durs [1 3 2 1]
                  :ratio 1
                  :tempo 60
                  :on-event (fn [x] (println x))}))

(defn maybe-reset-cycle [voice-data]
  (if-not (:reset-cycle? voice-data)
    voice-data
    (-> voice-data
        (assoc :cycle 0
               :cycle-delta 0
               :elapsed-dur 0
               :cycle-len (get-cycle-len (assoc voice-data
                                                :recalculate-cycle-len?
                                                (not (:keep-cycle-len? voice-data)))))
        (dissoc :reset-cycle?))))

(defn update-time-data [voice-data]
  (-> voice-data
      maybe-reset-cycle
      calculate-next-voice-state))

(defn update-voice-config-data
  [{:keys [update update?] :as voice-data}]
  (let [update* (cond (and update? update) (if (update? voice-data) update nil)
                      update update
                      :else nil)]
    (cond-> voice-data
      update* (-> (merge update*)
                  (dissoc :update? :update))
      true (#(assoc % :cycle-len (get-cycle-len %))))))

(defn update-voice-state!
  [voice-atom]
  (swap! voice-atom
         (fn [voice-data] (-> voice-data
                              update-voice-config-data
                              update-time-data))))

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
(declare schedule!)

(defn after-event [voice-atom]
  (let [{:keys [playing?] :as _voice-data} (update-voice-state! voice-atom)]
    (when playing? (schedule! voice-atom))))

(defn on-error [voice-atom error]
  (timbre/error error)
  (let [v* (deref voice-atom)
        prev-on-event (:prev-on-event v*)
        on-startup-error (:on-startup-error v*)]
    (cond prev-on-event (try
                          (prev-on-event {:data v*
                                          :voice voice-atom
                                          :dur (-> v* :current-event :dur)})
                          (swap! voice-atom :on-event prev-on-event)
                          #?(:clj (catch Exception _ (timbre/error "Can not recover from error"))
                             :cljs (catch js/Error _ (timbre/error "Can not recover from error")))
                          (finally (after-event voice-atom)))

          (zero? (:index v*)) (on-startup-error)
      ;; probably when there is no `:prev-on-event` and `:index` is not zero.
          :else (after-event voice-atom))))

(defn get-on-event-fn [voice-atom]
  (or (and (not (:update? (:update @voice-atom)))
           (:on-event (:update @voice-atom)))
      (:on-event @voice-atom)))

(defn schedule! [voice-atom]
  (let [schedule  (-> voice-atom deref :current-event :start-at)
        on-event* (fn []
                    (let [{:keys [loop? durs current-event] :as v*} (deref voice-atom)
                          ;; the current event:
                          {:keys [index]} current-event]
                      (when (:playing? v*)
                        (timbre/debug "About to play event" index :durs durs :loop loop?)
                        (try (when (play-event? index durs loop?)
                               (let [on-event (get-on-event-fn voice-atom)]
                                 (on-event {:event current-event
                                            :voice v*
                                            :voice-atom voice-atom})))
                             (after-event voice-atom)
                             #?(:clj (catch Exception e (on-error voice-atom e))
                                :cljs (catch js/Error e (on-error voice-atom e)))))))]
    (apply-at schedule on-event*)))

(defn calculate-cycle-delta [elapsed-dur cycle-len]
  (/ (mod elapsed-dur cycle-len)
     cycle-len))

(defn get-cycle-len
  [{:keys [durs ratio cycle-len recalculate-cycle-len?] :as _config}]
  (cond
    (and (fn? durs) (not cycle-len)) (do (timbre/warn "`durs` is a function but no `cycle-len` has been provided, defaulting to 1*ratio")
                                         (* ratio 1))
    (and cycle-len (not recalculate-cycle-len?)) cycle-len
    :else (* ratio (apply + durs))))

(defn get-cycle-data
  [{:keys [elapsed-dur] :as config
    :or {elapsed-dur 0}}]
  (let [cycle-len (get-cycle-len config)]
    {:cycle-len cycle-len
     :cycle (quot elapsed-dur cycle-len)
     :cycle-delta (calculate-cycle-delta elapsed-dur cycle-len)}))

(defn get-current-dur-data-multi-pred
  [{:as voice-data :keys [durs]}]
  (cond
    (sequential? durs) :durs-vector
    (fn? durs) :durs-gen-fn
    :else (throw (ex-info "Unknown dur-data, cannot `get-current-dur-data-multi-pred`" voice-data))))

(defmulti get-current-dur-data
  #'get-current-dur-data-multi-pred)

(defmethod get-current-dur-data :durs-vector
  [{:keys [tempo ratio durs index] :as _voice-data}]
  (let [dur (-> (wrap-at index durs) (* ratio))
        event-dur (dur->ms dur tempo)]
    {:dur dur :event-dur event-dur}))

(defmethod get-current-dur-data :durs-gen-fn
  [{:keys [durs tempo] :as voice-data}]
  (let [dur (durs voice-data)
        event-dur (dur->ms dur tempo)]
    {:dur dur :event-dur event-dur}))

(comment
  (init-voice-data {:durs [1 2 3]
                    :on-event println
                    :ratio 1}))

(defn play!
  "Gets a config and returns a `voice-atom` that will start playing,
  if `playing?` is true (default)"
  [config]
  (let [voice (init-voice-data config)]
    (schedule! voice)
    voice))
(comment
  (def v1 (play! {:durs [1 2 3]
                  :on-event println
                  :ratio 1})))

(defn stop! [voice-atom]
  (swap! voice-atom assoc :playing? false))

(comment
  (stop! v1)
  (require '[overtone.core :as o])

  (schedule! nil)
  (o/stop)
  (timbre/set-level! :debug))
