(ns nanc-in-a-can.una-semana-de-bondad
  (:require [overtone.live :refer :all]
            [nanc-in-a-can.core :refer [converge]]
            [nanc-in-a-can.sequencing :refer [sequencer]]))


(defn rand-pos [smpl] (rand-int (:n-samples smpl)))

(defn dur->ms [dur bpm] (* dur (/ 60 bpm)))


(defn nthw [coll index not-found]
  (try
    (let [i (mod index (count coll))]
      (nth coll i))
    (catch Throwable _  not-found)))

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
(comment
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
  (stop))



(recording-start "~/Desktop/orbitales-canon-v1.wav")
(recording-stop)
