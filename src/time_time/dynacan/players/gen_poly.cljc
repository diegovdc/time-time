(ns time-time.dynacan.players.gen-poly
  (:require [taoensso.timbre :as log]
            [time-time.dynacan.core :refer [find-relative-voice-first-event]]
            [time-time.sequencing-3 :as s]
            [time-time.standard :refer [now]]
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


(comment
  (def v (s/play! [1 1 1 1]
                  (fn [{:keys [data]}]
                    (s (wrap-at (data :index)
                                [300 400]))) :loop? true ))
  (swap! v assoc :loop? false))

(defonce refrains (atom {}))


(defmacro on-event [f-body]
  (list 'fn '[{{:keys [index]} :data}] f-body))

(defn update-refrain
  ([id key val] (update-refrain id #(assoc % key val)))
  ([id update-fn]
   (swap! refrains update id #(do (swap! % update-fn) %))))

(defn ref-rain [& {:keys [id durs on-event loop? ref distance]
                   :or {loop? true
                        distance 1}
                   :as config}]
  (cond (and (@refrains id) (-> @refrains id deref :playing?))
        (update-refrain id #(merge % config))
        (and (@refrains ref) (-> @refrains ref deref :playing?))
        (let [voice (add-to! (-> @refrains ref deref) distance on-event config)]
          (swap! refrains assoc id voice))
        :else
        (let [voice (s/play! durs on-event :loop? loop?)]
          (swap! refrains assoc id voice))))

(comment
  (do (ref-rain :id ::v1
                :durs [1/3 1/2 1/5]
                :on-event (on-event (s 833))
                :loop? false)
      (ref-rain :id ::v2
                :ref ::v1
                :durs [1/2 1/2 1/2 1]
                :cp 1
                :on-event (on-event (s (rand-nth [700])))
                :ratio 1/3
                :loop? false)))
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

(defn define-ref-voice [x]
  (def x 5))

(define-ref-voice 'g )

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

(get @refrains "h")

(reset! refrains {})

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
                                                        ~(list 'fn '[{{:keys [index ]} :data}]
                                                               on-event))) )
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
                          ~opts*
                          ))))
        (list 'add-to-refrain (str symbol*) symbol*)))

#_(refrain h
           :durs [1/2]
           :on-event (do (do (log/info "on-event called" index (now)) (s)) 9)
           :ratio 1
           :loop? true
           :start-index 0
           :tempo 60)
(comment
  (swap! h assoc :durs [1/2])

  (name `h)
  (swap! h assoc :durs [1])
  (-> refrains deref)
  (reset! refrains {})
  (swap! h  merge {:durs [1 2]})
  (swap! refrains dissoc "h2"))

(def fib-scale [1.1459102934487975
                1.2360828548001543
                1.3262554161515112
                1.3819820590666498
                1.4721546204180067
                1.5278812633331453
                1.583607906248284
                1.6180469715698396
                1.7082195329211964
                1.763946175836335
                1.8196728187514737
                1.8541118840730293
                1.909838526988168
                1.9442775923097235
                1.9787166576312791
                2.0000000000000004])

(comment
  (require '[overtone.core :refer :all :exclude [now on-event] :as o]
           '[time-time.standard :refer [wrap-at]])
  (boot-internal-server)
  (defsynth s [freq 327] (out 0 (pan2 (* 0.2
                                         (env-gen (env-perc)  :action FREE )
                                         (square freq)))))
  (s)
  (s 777)
  (refrain h3
           :ref h>>1
           :durs [1 1/3 1]
           :on-event (s (* (rand-nth [2 1]) (wrap-at index [777 888 600 500 666 400])))
           :ratio 3/8
           :loop? true
           :start-index 0)
  (refrain h2
           :ref h>>1
           :durs [1/5 1 1/5 1 1/5 1/8]
           :on-event (when  (= 1 (wrap-at index [1 0 0 0 0 0 0]))
                       (s (* (wrap-at index [21/3 8/2 5/3])
                             300
                             (wrap-at (* index 24) fib-scale))))
           :ratio (+ 1 (* 1/8 618/1000))
           :loop? false
           :start-index 0)
  (refrain h
           :durs [1/3 1/8 1/3 1/8]
           :on-event (do (log/info index (now))
                         (when  (= 1 (wrap-at index [0 0 0 0 0 0 0 0 0 0 1 ]))
                           (s (* 300
                                 #_(wrap-at index [8 5 3 13])
                                 (wrap-at (* (wrap-at index [1]) index) fib-scale)))))
           :ratio 1
           :loop? false
           :start-index 0
           :tempo 80)

  (stop)
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
