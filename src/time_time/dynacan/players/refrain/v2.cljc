(ns time-time.dynacan.players.refrain.v2
  "Uses sequencing-4"
  (:require
   [time-time.dynacan.core :refer [find-relative-voice-first-event]]
   [time-time.sequencing-4 :as s4]
   [time-time.standard :refer [wrap-at]]))

;; API
(declare before-update update-refrain maybe-run-on-stop-fn active-refrains add-to!)

(defonce refrains (atom {}))

(defn reset [] (reset! refrains {}))

(defn stop
  ([]
   (doseq [id (keys @refrains)]
     (update-refrain id :playing? false)
     (maybe-run-on-stop-fn (get @refrains id)))
   (reset))
  ([id]
   (cond (and (@refrains id) (:playing? (deref (@refrains id))))
         (do (update-refrain id :playing? false)
             (maybe-run-on-stop-fn (get @refrains id)))

         (@refrains id)
         (println "refrain already stopped")

         :else (println "Could not find refrain with id: " id))))

(defn ref-rain
  [& {:keys [id durs on-event loop? ref distance]
      :or {loop? true
           distance 1}
      :as config}]
  (let [existing-voice-config (@refrains id)
        existing-voice? (and existing-voice-config (:playing? @existing-voice-config))
        refrains* (cond
                    existing-voice? (let [config* (merge config (s4/get-cycle-data (merge @existing-voice-config config)))]
                                      (update-refrain id #(assoc %
                                                                 :update config*
                                                                 :refrain/config config*)))

                    (and (@refrains ref) (-> @refrains ref deref :playing?))
                    (let [voice (add-to! (-> @refrains ref deref) distance on-event config)]
                      (swap! voice assoc :refrain/config config)
                      (swap! refrains assoc id voice))

                    :else
                    (let [voice (s4/play! {:extra-data {:id id :ref ref}
                                           :durs durs
                                           :on-event on-event
                                           :loop? loop?
                                           :tempo (config :tempo 60)
                                           :ratio (config :ratio 1)
                                           :on-startup-error (fn [] (stop id))})]
                      (swap! voice assoc :refrain/config config)
                      (swap! refrains assoc id voice)))]
    (active-refrains refrains*)))

(comment
  (require '[overtone.music.time :refer [now]])
  (@refrains :hola)
  (ref-rain :id :hola
            :durs [1 2 3]
            :on-event (on-event
                       (println "hola")
                       (when (cyi? 3) (println "First" dur))))
  (ref-rain :id :adios
            :ref :hola
            ;; :reset-cycle? true
            :durs [2 3]
            :on-event (on-event
                       (println cycle cycle-delta-index cycle-delta)))

  (stop))

(defn at-index*
  "`offset` can be a function or a number"
  ([index coll] (wrap-at index coll))
  ([index offset coll] (if (fn? offset)
                         (wrap-at (offset index) coll)
                         (wrap-at (+ offset index) coll))))

(defn cycle-0-index?
  ([index cycle-0-index]
   (= index cycle-0-index))
  ;; NOTE in most cases this will work, when the `cycle-len` is derived from the `durs` sum; however in cases where this is not, the return value will not be correct.
  ([index cycle-0-index offset durs cycle-len]
   (= index (+ (mod  offset  (if-not (fn? durs) (count durs) cycle-len))
               cycle-0-index))))

(defmacro cyi?
  "To be used inside an on-event. If called without an argument, it will return true when the cycle is begining.

  With an `offset` argument it will attempt to calculate if the current index is an `offset` away from the start of the cycle.
  Using an `offset` is prone to returning false positives if the `cycle-len` has not been derived from the sum of `durs`."
  ([] `(~cycle-0-index? ~'index ~'cycle-0-index))
  ;; NOTE in most cases this will work, when the `cycle-len` is derived from the `durs` sum; however in cases where this is not, the return value will not be correct.
  ([offset] `(~cycle-0-index? ~'index ~'cycle-0-index ~offset ~'durs ~'cycle-len)))

(comment
  ((on-event (cyi?)) {:event {:index 0 :cycle-0-index 0}})
  ((on-event (cyi?)) {:event {:index 1 :cycle-0-index 0}})
  ((on-event (cyi?)) {:event {:index 1 :cycle-0-index 1}})
  ((on-event (cyi?)) {:event {:index 1 :cycle-0-index 0}
                      :voice {:durs [1 1 1] :cycle-len 3}}))

(defmacro on-event
  "Provides
  `index` (alias `i`),
  `dur` (original duration),
  `dur-s` (duration in seconds),
  `dur-ms` (duration in milliseconds) and
  `at-index` (alias `at-i`,function that get a value in a collection based on index, it wraps with `mod`)
              can take an `offset` as the first argument and the collection as the second, and `offset` can be a function or a number"
  [& forms]
  `(fn [~'{{:keys [index
                   dur
                   dur-ms
                   dur-s
                   cycle
                   cycle-delta
                   cycle-0-index
                   cycle-delta-index
                   new-cycle?] :as event} :event
           {:keys [id cycle-len durs] :as voice} :voice}]
     (let [~'data ~'event
           ~'i ~'index
           ~'cy ~'cycle
           ~'cy0i ~'cycle-0-index
           ;; ~'cy0? (= ~'index ~'cycle-0-index)
           ~'cyd ~'cycle-delta
           ~'cydi ~'cycle-delta-index
           ~'at-index (partial at-index* ~'index)
           ~'at-i ~'at-index]
       ~@forms)))
(comment
  (macroexpand-1 '(on-event (at-index [1 2 3])))
  ((on-event (at-index  #(- 1 %) [1 2 3])) {:data {:index 1}})
  ((on-event (at-i [1 2 3])) {:data {:index 1}})
  (on-event (at-index [1 2 3])
            (at-index [1 2 3])))

(defmacro when-mod
  "To be used within `on-event` as it assumes the existence of `index` in the context of the function"
  [modulo index-set f-call]
  `(when (~index-set (mod ~'index ~modulo))
     ~f-call))

(defn active-refrains [refrains]
  (->> refrains
       (filter (comp :playing? deref second))
       (map first)))

(defn add-to! [ref-voice events-from-cp on-event
               {:keys [id durs ratio loop? cp]
                :or {durs (ref-voice :durs)
                     ratio (ref-voice :ratio)
                     loop? (ref-voice :loop?)}
                :as config}]
  ;; TODO verify if ref-voice is playing?
  (let [start-time (+ (ref-voice :elapsed-ms) (ref-voice :started-at))
        tempo (ref-voice :tempo)
        {:keys [index elapsed cp cp-elapsed interval-from-cp events-from-cp]}
        (find-relative-voice-first-event events-from-cp ;; TODO should probably return the start time of the voice
                                         ref-voice
                                         {:durs (if (fn? durs) [1] durs) ;; TODO: this is a hardcoded quick solution, but it may be better to solve it more elegantly. Though the cases may vary, i.e. if a `durs` fn is `(rand)`, it may not make any sense to solve it. Anyways this solution allows a `ref-rain` that has a `:ref` to start without a problem (it crashed before).
                                          :cp cp
                                          :ratio ratio
                                          :loop? loop?})]
    #_(log/debug "adding voice... cp-at ref-voice index"
                 (ref-voice :index)
                 (+ events-from-cp (ref-voice :index)))
    (s4/play! {:durs durs
               :on-event  on-event
               :ratio ratio
               :start-index index
               :elapsed elapsed
               :tempo tempo
               :start-time start-time
               :loop? loop?
               :before-update before-update
               :extra-data {:id id
                            :ref (:id ref-voice)
                            :cp cp
                            :cp-at cp-elapsed
                            :interval-from-cp interval-from-cp
                            :events-from-cp events-from-cp}})))

(defn before-update
  [{:as data {dur :dur} :current-event}]
  (-> data
      (update :events-from-cp dec)
      (update :interval-from-cp - dur)))

;; TODO verificar cómo funciona el cp en `add-to`... cómo se estan leyendo las duraciones... tal vez rotación
;;  compilar en  diversos momentos
;;  en diversos loops

;; hacer pruebas (combinaciones de loops, corrección del cp)
;; pensar en tiempo de reposo (remainder) entre voces (que sea opcional)
;; pensar otros modos de control de las densidades* (quizá otra función aparte)
;; comenzar a pensar en la sintaxis

(defn backup-on-event [config]
  (assoc config :prev-on-event (:on-event config)))

(defn update-refrain
  ([id key val] (update-refrain id #(assoc % key val)))
  ([id update-fn]
   (swap! refrains update id #(do (swap! % (comp update-fn backup-on-event)) %))))

(defn- maybe-run-on-stop-fn
  [refrain]
  (let [on-stop (-> @refrain :refrain/config :on-stop)]
    (when on-stop (on-stop @refrain))))
