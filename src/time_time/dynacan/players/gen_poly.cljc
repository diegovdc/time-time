(ns time-time.dynacan.players.gen-poly
  (:require
   [time-time.dynacan.core :refer [find-relative-voice-first-event]]
   [time-time.sequencing-3 :as s]
   [time-time.standard :refer [now wrap-at]]
   [clojure.string :as str]))

(declare before-update)

(defn add-to! [ref-voice events-from-cp on-event
               {:keys [durs ratio loop? cp]
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
                                         {:durs durs :cp cp :ratio ratio :loop? loop?})]
    #_(log/debug "adding voice... cp-at ref-voice index"
                 (ref-voice :index)
                 (+ events-from-cp (ref-voice :index)))
    (s/play! durs
             on-event
             :ratio ratio
             :start-index index
             :elapsed elapsed
             :tempo tempo
             :start-time start-time
             :loop? loop?
             :before-update before-update
             :extra-data {:cp cp
                          :cp-at cp-elapsed
                          :interval-from-cp interval-from-cp
                          :events-from-cp events-from-cp})))

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

(comment
  (def v (s/play! [1 1 1 1]
                  (fn [{:keys [data]}]
                    (s (wrap-at (data :index)
                                [300 400]))) :loop? true))
  (swap! v assoc :loop? false))

(defn at-index*
  "`offset` can be a function or a number"
  ([index coll] (wrap-at index coll))
  ([index offset coll] (if (fn? offset)
                         (wrap-at (offset index) coll)
                         (wrap-at (+ offset index) coll))))

(defmacro on-event
  "Provides
  `index` (alias `i`),
  `dur` (original duration),
  `dur-s` (duration in seconds),
  `dur-ms` (duration in milliseconds) and
  `at-index` (alias `at-i`,function that get a value in a collection based on index, it wraps with `mod`)
              can take an `offset` as the first argument and the collection as the second, and `offset` can be a function or a number"
  [& forms]
  `(fn [~'{{:keys [index cycle cycle-delta dur dur-ms dur-s] :as data} :data}]
     (let [~'i ~'index
           ~'delta ~'cycle-delta
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

(comment
  (macroexpand-1 '(when-mod 5 #{0 3}
                            (println "hola")))
  (let [index 3] ;; `index` is expected in the context
    (when-mod 5 #{0 3} (println "hola")))

  ((on-event (when-mod 3 #{0} (println "hola " index))) {:data {:index 3}}))

(defonce refrains (atom {}))

(defn backup-on-event [config]
  (assoc config :prev-on-event (:on-event config)))

(defn update-refrain
  ([id key val] (update-refrain id #(assoc % key val)))
  ([id update-fn]
   (swap! refrains update id #(do (swap! % (comp update-fn backup-on-event)) %))))

(defn active-refrains [refrains]
  (->> refrains
       (filter (comp :playing? deref second))
       (map first)))

(declare stop)
(defn ref-rain [& {:keys [id durs on-event loop? ref distance]
                   :or {loop? true
                        distance 1}
                   :as config}]
  (let [existing-voice? (and (@refrains id) (-> @refrains id deref :playing?))
        refrains* (cond
                    existing-voice? (let [config* (merge config (s/get-cycle-data durs config))]
                                      (update-refrain id #(assoc %
                                                                 :update config*
                                                                 :refrain/config config*)))

                    (and (@refrains ref) (-> @refrains ref deref :playing?))
                    (let [voice (add-to! (-> @refrains ref deref) distance on-event config)]
                      (swap! voice assoc :refrain/config config)
                      (swap! refrains assoc id voice))

                    :else
                    (let [voice (s/play! durs on-event :loop? loop?
                                         :tempo (config :tempo 60)
                                         :ratio (config :ratio 1)
                                         :on-startup-error (fn [] (stop id)))]
                      (swap! voice assoc :refrain/config config)
                      (swap! refrains assoc id voice)))]
    (active-refrains refrains*)))
(comment
  ;;  Tests for different errors on ref-rain/sequencing-3 stuff
  (stop)
  (def sec [1 2])
  (-> @refrains) ;; an empty vector will throw an error
  ;; A function
  (ref-rain
   :id :hola
   :durs [1 2]
   :on-event (on-event
              (println "holas" cycle)
                ;; throw at some point of the execution

              #_(throw (ex-info "ups" {}))))
  (ref-rain
   :id :bola
   :durs [1]
   :on-stop (fn [_] (println "stopping" _))
   :on-event (on-event
              (println "bolas" index)
              #_(throw (ex-info "ups" {})))))

(comment
  (require '[overtone.core :refer :exclude [now on-event] :as o]
           '[time-time.standard :refer [wrap-at]])
  (o/boot-internal-server)
  (o/defsynth s [freq 327] (o/out 0 (o/pan2 (* 0.2
                                               (o/env-gen (o/env-perc)  :action o/FREE)
                                               (o/square freq)))))
  (s))

(comment
  (do (ref-rain :id ::v4
                :durs [1/3 1/2 1/5]
                :on-event (on-event (s 833))
                :loop? true))
  (ref-rain :id ::v2
            :ref ::v4
            :durs [1/2 1/2]
            :cp 1
            :on-event (on-event (s (wrap-at index [701 700])))
            :ratio 1/20
            :loop? true))
(comment (o/stop))
#_(add-to! (deref (@refrains ::v1)) 2 (on-event (s 700))  {:durs [1 2] :ratio 1/3 :loop? true})
#_(add-to! (deref (@refrains ::v1)) 3
           (on-event (when (= 0 (mod index 10))
                       (s (rand-nth [333 400]))))
           {:durs [2 3] :ratio 1/5 :loop? true})

(def upref update-refrain)

(comment (upref ::v1 :loop? false)

         (update-refrain ::v1 #(assoc % :loop? false)))

(comment (defvoice one 'xoxoxox :ratio 2
           (mod0 3 index
                 (fx (stut 2) (bd) (reverb 0.5))
                 (clap)))

         (defvoice two 'xoxoxox :ratio 1/3 :ref one (clap)))
;; TODO rename me

(defn add-to-refrain [key refrain]
  (swap! refrains assoc key refrain))

(defmacro refrain-a [symbol* & {:keys [durs on-event ref] :as opts}]
  #_(println (get @refrains (-> ref parse-ref first))
             (-> ref parse-ref second read-string inc))
  ;; TODO path for ref-voice
  (list 'def symbol*
        (let [opts* (apply concat (dissoc opts :on-event :ref))]
          `(~s/play!
            ~durs
            ~(list 'fn '[{{:keys [index]} :data}] on-event)
            ~@opts*))))

(defn update-voice [symbol* opts]
  (println opts)
  (swap! (get @refrains (str symbol*)) merge opts))

(keys @refrains)

(defn reset [] (reset! refrains {}))
(defn- maybe-run-on-stop-fn
  [refrain]
  (let [on-stop (-> @refrain :refrain/config :on-stop)]
    (when on-stop (on-stop @refrain))))
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

(defn parse-ref [ref] (str/split (str ref) #">>"))

(defmacro refrain [symbol* & {:keys [durs on-event ref] :as opts}]
  #_(println (get @refrains (-> ref parse-ref first))
             (-> ref parse-ref second read-string inc))
  ;; TODO path for ref-voice
  (list 'do
        (cond
          (get @refrains (str symbol*)) (list 'update-voice (str symbol*)
                                              (let [opts* (-> opts (dissoc :on-event :ref))]
                                                `(assoc ~opts* :on-event
                                                        ~(list 'fn '[{{:keys [index]} :data}]
                                                               on-event))))
          (not ref)
          ;; directly call s/play!
          (list 'def symbol*
                (let [opts* (apply concat (dissoc opts :on-event :ref))]
                  `(~s/play!
                    ~durs
                    ~(list 'fn '[{{:keys [index]} :data}] on-event)
                    ~@opts*)))
          :else (list 'def symbol*
                      (let [opts* (-> opts (dissoc :on-event :ref))]
                        `(~add-to!
                          ~(deref (get @refrains (-> ref parse-ref first)))
                          ~(-> ref parse-ref second read-string)
                          ~(list 'fn '[{{:keys [index]} :data}] on-event)
                          ~opts*))))
        (list 'add-to-refrain (str symbol*) symbol*)))
