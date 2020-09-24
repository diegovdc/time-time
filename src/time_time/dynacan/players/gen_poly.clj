(ns time-time.dynacan.players.gen-poly
  (:require [taoensso.timbre :as log]
            [time-time.dynacan.core :refer [find-relative-voice-first-event]]
            [time-time.sequencing-3 :as s]
            [time-time.standard :refer [now]]))

(declare before-update)

(defn add-to! [ref-voice events-from-cp on-event {:keys [durs ratio loop?] :as config}]
  ;; TODO verify if ref-voice is playing?
  (let [start-time (+ (ref-voice :elapsed-ms) (ref-voice :started-at))
        tempo (ref-voice :tempo)
        {:keys [index elapsed cp cp-elapsed interval-from-cp events-from-cp]}
        (find-relative-voice-first-event events-from-cp ;; TODO should probably return the start time of the voice
                                         ref-voice
                                         config)]
    (log/debug "adding voice... cp-at ref-voice index"
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






(comment (defvoice one 'xoxoxox :ratio 2
           (mod0 3 index
                 (fx (stut 2) (bd) (reverb 0.5))
                 (clap)))

         (defvoice two 'xoxoxox :ratio 1/3 :ref one (clap)))


(defmacro refrain [symbol durs ratio on-event]
  (list 'def symbol `(s/play! ~durs
                              (list 'fn '[{:keys [data]}]
                                    ~on-event)
                              :loop? false
                              :ratio ~ratio)))

(comment
  (refrain one [1 1 1] 1 (log/info "v1" data (now)))
  (-> one)

  (swap! one assoc :playing? false))

(defn mock-splay [durs on-event & {:keys [ratio] :as opts}]
  (log/info ratio opts)
  (on-event {:data {:index 1}}))

(defmacro prepare-on-event
  "`body` is the body of a function. It is syntax sugr and assumes there are two
  vars in context, which are actually passed as args:
  `ev` for the event data, and
  `st` for the state data"
  [body]
  (let [f# (list 'fn '[{:keys [data]}] body)]
    f#))

(comment
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
                        :events-from-cp events-from-cp}))
;; TODO rename me
(defmacro refrain [symbol & {:keys [durs on-event] :as opts}]
  ;; TODO path for ref-voice
  (list 'def symbol
        (let [opts* (apply concat (dissoc opts :on-event))]
          `(~s/play!
            ~durs
            ~(list 'fn '[{{:keys [index]} :data}] on-event)
            ~@opts*))))


(comment
  (require '[overtone.core :refer :all :exclude [now]])
  (def s (synth (out 0 (* (env-gen (env-perc)) (sin-osc 200)))))
  (refrain h
           :durs [1 2 3]
           :on-event (do (do (log/info "on-event called" (now)) (s)) 9)
           :ratio 1
           :start-index 0
           :tempo 60)
  (defn prolong
    "Calculate nearest position if voice would be playing"
    ;; WIP
    [v]
    ;; NOTE Pseudo-code, doesn't work!
    (loop [v* v]
      (if (>= (:elapsed v*) (now))
        v*
        (recur (get-next-event v))))))
(now)
(comment
  ;; [[durs]]
;;;  ref-voice
;;; find-first sirve para durs distintas

  (gen-poly [{:r 1 :xo 'xoxoxox :ref? true}
             {:r 1 :durs [3 4] :cp 2}
             {:r 2 :durs [1 2 3 4] :cp 1}])

  (def v1 (s/play! [1 1 1]
                   (fn [{:keys [data]}] (log/info "v1" (:index  data) (now)))
                   :loop? true
                   :ratio 1))
  (refrain :durs [1 1 1]
           :on-event (log/info "v1" dur index (now))
           :fx (-> x y z)
           :loop? true
           :ratio 1)

  (def v2 (add-to! @v1 5
                   (fn [{:keys [data]}] (log/info "v2" (:index  data) (now)))
                   {:durs [1 2] :ratio 1/3 :cp 0 :loop? false}))

  #_(defn add-to! [v ech on-event config]
      (if (v :playing?)
        (add-to*! v ech on-event config)
        (throw )))

  (do (swap! v1 assoc :playing? false)
      (swap! v2 assoc :playing? false))

  #_(def ref-voice (play! [1 2 3 5 6]))

  (def voice-state {:durs [1],
                    :index 2 ;; sucede en: + started-at elapsed 750
                    :started-at 1597195400418
                    :current-event {:dur-ms 250N, :dur 1/4}
                    :playing? true
                    :ratio 1/4
                    :loop? true
                    :tempo 60
                    :elapsed 500N} )


  (add-to v1 :id 1 2 (fn [_] (println "segunda voz")))

  cp @ v1 (- 5 2)



  (defn add-to [ref-voice id cp ratio on-event])

  [4 7]

  (def second-voice (add-to! ref-voice :id :v1 :cp-from (rand-int 4) :durs [1 2 3 4] :loop? false))

  (def third-voice (add-to! ref-voice :cp 3 :durs [1 2 3 4]))

  (hush! :v1)
  )
