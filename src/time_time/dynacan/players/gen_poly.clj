(ns time-time.dynacan.players.gen-poly
  (:require [taoensso.timbre :as log]
            [time-time.dynacan.core :refer [find-relative-voice-first-event]]
            [time-time.sequencing-3 :as s]
            [time-time.standard :refer [now]]))

#_(-> @v1)
#_(let [durs [1 2]
        ratio 1/3
        echoic-distance-qty 5
        *v1 @v1
        {:keys [index elapsed] :as first-event}
        (find-relative-voice-first-event echoic-distance-qty
                                         *v1
                                         {:durs durs :ratio ratio :loop? true})

        second-voice-at-cp-event (last (get-next-n-events
                                        durs
                                        first-event
                                        (first-event :echoic-distance-event-qty)))

        ref-voice-cp-event (last (get-next-n-events (*v1 :durs)
                                                    (log/spy :info (assoc *v1 :echoic-distance 10))
                                                    echoic-distance-qty))]
    (pprint ref-voice-cp-event)
    (pprint second-voice-at-cp-event)
    (def v2 (s/play! durs
                     (fn [{:keys [data]}] (log/info "v2" (:index  data) (now)))
                     :ratio ratio
                     :start-index index
                     :elapsed elapsed
                     :start-time (+ (*v1 :elapsed) (*v1 :started-at))
                     :loop? true
                     )))

(declare before-update)

(defn add-to! [ref-voice echoic-distance-qty on-event {:keys [durs ratio loop?] :as config}]
  ;; TODO verify if ref-voice is playing?
  (let [start-time (+ (ref-voice :elapsed) (ref-voice :started-at))
        tempo (ref-voice :tempo)
        {:keys [index elapsed cp cp-elapsed-at echoic-distance echoic-distance-event-qty]}
        (find-relative-voice-first-event echoic-distance-qty ;; TODO should probably return the start time of the voice
                                         ref-voice
                                         config)]
    (log/info "adding voice... cp-at ref-voice index"
              (ref-voice :index)
              (+ echoic-distance-qty (ref-voice :index)))
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
                          :cp-at cp-elapsed-at
                          :echoic-distance echoic-distance
                          :echoic-distance-event-qty echoic-distance-event-qty})))

(defn before-update
  [{:as data {dur :dur} :current-event}]
  (-> data
      (update :echoic-distance-event-qty dec)
      (update :echoic-distance - dur)))

;; TODO
;; verificar cómo funciona el cp en `add-to`... cómo se estan leyendo las duraciones... tal vez rotación
;;  compilar en  diversos momentos
;;  en diversos loops


;; hacer pruebas (combinaciones de loops, corrección del cp)
;; pensar en tiempo de reposo (remainder) entre voces (que sea opcional)
;; pensar otros modos de control de las densidades* (quizá otra función aparte)
;; comenzar a pensar en la sintaxis


(comment
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

  (gen-poly [{:r 1 :durs [1 2 3 4] :ref? true}
             {:r 1 :durs [3 4] :cp 2}
             {:r 2 :durs [1 2 3 4] :cp 1}])

  (def v1 (s/play! [1 1 1]
                   (fn [{:keys [data]}] (log/info "v1" (:index  data) (now)))
                   :loop? true
                   :ratio 1))

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
