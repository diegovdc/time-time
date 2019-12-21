(ns nanc-in-a-can.una-semana-de-bondad
  (:require [overtone.core :refer :all]
            [nanc-in-a-can.core :refer [converge]]
            [nanc-in-a-can.sequencing :refer [sequencer]]))


(defn rand-pos [smpl] (rand-int (:n-samples smpl)))

(defn dur->ms [dur bpm] (* dur (/ 60 bpm)))


(defn nthw [coll index not-found]
  (try
    (let [i (mod index (count coll))]
      (nth coll i))
    (catch Throwable _ (user/spy :not-found not-found))))

(declare xos)
(defn xo-play? [index tempo-index]
  (let [current-event (nthw xos index false)]
    (if (boolean? current-event)
      current-event
      ((set current-event) tempo-index))))

(def o1 (audio-bus 2))


(defonce main-g (group "get-on-the-bus main"))
(defonce early-g (group "early birds" :head main-g))
(defonce later-g (group "latecomers" :after early-g))
(def x true)
(def o false)

(comment
  (defsynth out100 [in* 99 amp 1] (out 0 (* amp (in in*))))
  (defonce outy (out100  :fx-group later-g)))

(declare rate)

(declare silence)
(declare play-sample*)
(defsynth play-sample*
  [smpl silence
   a 3
   r 3
   dur 0
   pan 0
   start-pos 0
   rate 1
   bp-freq 1000
   bp-q 1
   out* 0]
  (let [env (envelope [0 1 0.1 0] [a (* 3 dur) r 1] :lin)]
    (out out* (pan2 (* 7
                       ;; (sin-osc:kr (+ 100 (rand 30)))
                       ;; (sin-osc:kr 0.2)
                       (env-gen env :action FREE)
                       (bpf:ar
                        (play-buf:ar 1 smpl
                                     :rate rate
                                     :start-pos (min 0 (- (rand 100000) start-pos))
                                     :loop (if (= smpl silence)
                                             false
                                             true))
                        bp-freq
                        bp-q))
                    pan))))

(defn smpl-playa [vals index nome samples sample-sequence]
  (let [at-idx (get @sample-sequence index)
        smpl (or (:smpl at-idx) (nthw (deref samples) index silence))
        start-pos (or (:start-pos at-idx) (rand-pos smpl))
        ]
    (when (nil? at-idx)
      (swap! sample-sequence #(assoc % index {:start-pos start-pos
                                              :smpl smpl})))

    (when (xo-play? index (:tempo-index vals))
      (play-sample* [:tail early-g]
                    smpl
                    ;; dur + 2 secs for fade-in/out
                    (user/spy (+ 0 (dur->ms (:dur vals)
                                            (metro-bpm nome))))
                    :rate (rate vals index)
                    :out* 0
                    :a (+ 1 (rand 1))
                    :r (+ 2 (rand 5))
                    :bp-freq (+ 20 (rand-int 16000))
                    :bp-q (max 0.01 (rand 0.2))
                    :start-pos start-pos))))

(defn sample-canon [nome samples canon]
  (let [sample-sequence (atom {})]
    (->> canon
         (map (fn [voice] (sequencer
                          nome
                          voice
                          (fn [vals index]
                            (#'smpl-playa vals index nome samples sample-sequence))))))))



(defn semi-kill [group]
  (->> group
       (map
        (fn [atm] (stop-player (:next-event @atm))
          (swap! atm #(assoc % :stop? true))
          (:next-event @atm)))
       ))

(def silence (freesound-sample 459659))

(def orbitales (load-sample"/media/diego/Music/music/taller-abierto/instrumentos-1/renders/orbitales.wav"))

;; (def caminata (load-sample "/Users/user/sc/overtone/nanc-in-a-can/resources/caminata.wav"))
;; (def caminata2 (load-sample "/Users/user/sc/overtone/nanc-in-a-can/resources/caminata2.wav"))
;; (def caminata3 (load-sample "/Users/user/sc/overtone/nanc-in-a-can/resources/caminata3.wav"))
;; (def caminata4 (load-sample "/Users/user/sc/overtone/nanc-in-a-can/resources/caminata4.wav"))
;; (def caminata5 (load-sample "/Users/user/sc/overtone/nanc-in-a-can/resources/caminata5.wav"))
;; (def caminata6 (load-sample "/Users/user/sc/overtone/nanc-in-a-can/resources/caminata6.wav"))
;; (def engine (load-sample "/Users/user/sc/overtone/nanc-in-a-can/resources/164072__laurent__engine-underwater.wav"))
;; (def sub-sea-drilling (load-sample "/Users/user/sc/overtone/nanc-in-a-can/resources/271529__augustsandberg__sub-sea-drilling.wav"))
;; (def restaurant (load-sample "/Users/user/sc/overtone/nanc-in-a-can/resources/422097__felix-blume__restaurant-atmosphere-crowded.wav"))
;; (def human [caminata
;;             caminata2
;;             caminata3
;;             caminata4
;;             caminata5
;;             caminata6
;;             engine
;;             sub-sea-drilling
;;             restaurant])

;; (def glass-bottle (load-sample "/Users/user/sc/overtone/nanc-in-a-can/resources/110160__cognito-perceptu__glass-bottle-tinking-against-rocks.wav"))
;; (def human-nature [glass-bottle])

;; (def poeta-cerro-pasco (load-sample "/Users/user/sc/overtone/nanc-in-a-can/resources/175865__antigonia__poeta-cerro-de-pasco.aiff"))

;; (def communities [poeta-cerro-pasco])
;; (def cicadas (load-sample "/Users/user/sc/overtone/nanc-in-a-can/resources/163688__cognito-perceptu__cicadas.wav"))
;; (def glacier (load-sample "/Users/user/sc/overtone/nanc-in-a-can/resources/231525__thalamus-lab__iceberg-fragment-svalbard.wav"))
;; (def iceberg (load-sample "/Users/user/sc/overtone/nanc-in-a-can/resources/268023__dheming__breaking-ice-01.wav"))
;; (def glacier1 (load-sample "/Users/user/sc/overtone/nanc-in-a-can/resources/376184__drelliott0net__2015-04-09-svinafellsjokull-glacier-top.wav"))
;; (def dolphins (load-sample "/Users/user/sc/overtone/nanc-in-a-can/resources/408555__felix-blume__amazonian-dolphins.wav"))
(def whales (freesound-sample 322539))

(def birds (freesound-sample 467096))





;; (def nature [cicadas
;;              glacier
;;              iceberg
;;              glacier1
;;              dolphins
;;              whales])


(def m-rand (memoize (fn [_] (rand))))
(def m-rand2 (memoize rand))
(def xos
  (shuffle (concat
            (repeat 1 true)
            (repeat 200 false))))

(defn rate [vals index] (or
                         ;; 1
                         (+ 1 (* 0.1 (m-rand2 (:tempo-index vals) ))
                            (m-rand (:tempo-index vals))
                            )
                         ;; (inc (* index 0.1))
                         1))
(birds)
(stop)
(comment

  (do (def human-c (sample-canon (metronome 60)
                                 (shuffle [
                                        ;caminata
                                        ;caminata2
                                           ;; caminata3
                                           ;; caminata4
                                           ;; caminata5
                                           ;; caminata6
                                           ;; engine
                                           ;; sub-sea-drilling
                                        ;restaurant
                                           ])
                                 (converge {:durs (repeat 100 2)
                                            :tempos (map #(/ % 7) (range 7 10))
                                            :cps [70 90]})))
      human-c)
  p
  (semi-kill human-c)
  (stop)

  (def nat (atom []))
  (reset! nat (shuffle [
                                        ; ;   cicadas
                        ;; glacier
                        ;; iceberg
                        ;; glacier1
                        ;; dolphins
                        ;; birds
                        orbitales
                        ;; whales
                        ]))

  (do (def nature-c (sample-canon (metronome 30)
                                  nat
                                  (converge {:durs (flatten (repeat 1420 [1]))
                                             :tempos (map #(/ % 7) (range 7 39))
                                             :cps [300 570 890 1000 1200]})))
      nature-c)

  (semi-kill nature-c)
  (stop)

  (do (def human-nature-c (sample-canon (metronome 120)
                                        human-nature
                                        ;; 12 87
                                        (converge {:durs  (flatten (repeat 12 [1 4 3 5 6 7 2]))
                                                   :tempos (map #(/ % 7) (range 7 15))
                                                   :cps [70]})))
      human-nature-c)

  (do (def all-c (sample-canon (metronome 320)
                               (shuffle (concat human nature))
                               (converge {:durs (flatten (repeat 20 [1 4 3 5 6 7 2]))
                                          :tempos (map #(/ % 7) (range 7 15))
                                          :cps [70]})))
      all-c)

  (semi-kill all-c)
  (stop)
  )



(recording-start "~/Desktop/orbitales-canon-v1.wav")
(recording-stop)
