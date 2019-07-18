(ns nanc-in-a-can.una-semana-de-bondad
  (:require [overtone.live :refer :all]
            [nanc-in-a-can.core :refer [converge]]
            [nanc-in-a-can.sequencing :refer [sequencer]]))

(def nome (metronome 60))

(def silence (freesound-sample 459659))


(def caminata (load-sample "/Users/user/sc/overtone/nanc-in-a-can/resources/caminata.wav"))
(def caminata2 (load-sample "/Users/user/sc/overtone/nanc-in-a-can/resources/caminata2.wav"))
(def caminata3 (load-sample "/Users/user/sc/overtone/nanc-in-a-can/resources/caminata3.wav"))
(def caminata4 (load-sample "/Users/user/sc/overtone/nanc-in-a-can/resources/caminata4.wav"))
(def caminata5 (load-sample "/Users/user/sc/overtone/nanc-in-a-can/resources/caminata5.wav"))
(def caminata6 (load-sample "/Users/user/sc/overtone/nanc-in-a-can/resources/caminata6.wav"))
(def engine (load-sample "/Users/user/sc/overtone/nanc-in-a-can/resources/164072__laurent__engine-underwater.wav"))
(def sub-sea-drilling (load-sample "/Users/user/sc/overtone/nanc-in-a-can/resources/271529__augustsandberg__sub-sea-drilling.wav"))
(def restaurant (load-sample "/Users/user/sc/overtone/nanc-in-a-can/resources/422097__felix-blume__restaurant-atmosphere-crowded.wav"))
(def human [caminata
            caminata2
            caminata3
            caminata4
            caminata5
            caminata6
            engine
            sub-sea-drilling
            restaurant])


(def glass-bottle (load-sample "/Users/user/sc/overtone/nanc-in-a-can/resources/110160__cognito-perceptu__glass-bottle-tinking-against-rocks.wav"))
(def human-nature [glass-bottle])

(def poeta-cerro-pasco (load-sample "/Users/user/sc/overtone/nanc-in-a-can/resources/175865__antigonia__poeta-cerro-de-pasco.aiff"))

(def communities [poeta-cerro-pasco])

(def cicadas (load-sample "/Users/user/sc/overtone/nanc-in-a-can/resources/163688__cognito-perceptu__cicadas.wav"))
(def glacier (load-sample "/Users/user/sc/overtone/nanc-in-a-can/resources/231525__thalamus-lab__iceberg-fragment-svalbard.wav"))
(def iceberg (load-sample "/Users/user/sc/overtone/nanc-in-a-can/resources/268023__dheming__breaking-ice-01.wav"))
(def glacier1 (load-sample "/Users/user/sc/overtone/nanc-in-a-can/resources/376184__drelliott0net__2015-04-09-svinafellsjokull-glacier-top.wav"))
(def dolphins (load-sample "/Users/user/sc/overtone/nanc-in-a-can/resources/408555__felix-blume__amazonian-dolphins.wav"))
(def whales (freesound-sample 322539))

(def nature [cicadas
             glacier
             iceberg
             glacier1
             dolphins
             whales])



(defn rand-pos [smpl] (rand-int (:n-samples smpl)))

(defn dur->ms [dur bpm] (* dur (/ 60 bpm)))



(comment (play-sample* whales-1 10)         
         (stop))




(comment
  (do
    (def sample-sequence (atom {}))

    (->> @user/data :canon
         (map (fn [voice] (sequencer 
                          nome 
                          voice
                          (fn [vals index]
                            (let [at-idx (get @sample-sequence index)
                                  smpl (or (:smpl at-idx) whales-1)
                                  start-pos (or (:start-pos at-idx) (rand-pos smpl))]
                              (when (nil? at-idx)
                                (swap! sample-sequence #(assoc % index {:start-pos start-pos
                                                                        :smpl smpl})))
                              (play-sample* smpl                            
                                            ;; dur + 2 secs for fade-in/out
                                            (+ 2 (dur->ms (:dur vals) 
                                                          (metro-bpm nome)))
                                            ;:rate (inc (* index 0.2))
                                            :start-pos start-pos)))))))))



(defn nthw [coll index not-found]
  (try
    (let [i (mod index (count coll))]
      (nth coll i))
    (catch Throwable _ (user/spy :not-found not-found))))

(definst play-sample* [smpl silence dur 0 pan 0 start-pos 0 rate 1] 
  (let [env (envelope [0 1 0] [1 dur 1.2] :lin)]
    (pan2 (* 0.7
             ;; (sin-osc:kr (+ 20 (rand 300)))
             (sin-osc:kr 0.2)
             (env-gen env :action FREE) 
             (play-buf:ar 1 smpl
                          :rate rate
                          :start-pos start-pos 
                          :loop (if (= smpl silence) 
                                  false 
                                  true))) 
          pan)))




(defn smpl-playa [vals index samples sample-sequence]
  (let [at-idx (get @sample-sequence index)
        smpl (or (:smpl at-idx) (nthw samples index silence))
        start-pos (or (:start-pos at-idx) (rand-pos smpl))
        ]
    ;(user/spy (:name smpl))
    (when (nil? at-idx)
      (swap! sample-sequence #(assoc % index {:start-pos start-pos
                                              :smpl smpl})))
    (play-sample* smpl                            
                  ;; dur + 2 secs for fade-in/out
                  (+ 2 (dur->ms (:dur vals) 
                                (metro-bpm nome)))
                  :rate (inc (* index 0.1))
                  :start-pos start-pos)))

(defn sample-canon [nome samples canon]
  (let [sample-sequence (atom {})]
    (->> canon
         (map (fn [voice] (sequencer 
                          nome 
                          voice
                          (fn [vals index] 
                            (#'smpl-playa vals index samples sample-sequence))))))))
(comment


  (do (def human-c (sample-canon (metronome 220) 
                                 (shuffle [caminata
                                           caminata2
                                           caminata3
                                           caminata4
                                           caminata5
                                           caminata6
                                           engine
                                           sub-sea-drilling
                                           restaurant]) 
                                 (converge {:durs (shuffle (flatten (repeat 20 [1 4 3 5 6 7 2])))
                                            :tempos (map #(/ % 7) (range 7 15))
                                            :cps [70 130]})))
      human-c)

  (do (def nature-c (sample-canon (metronome 120) 
                                  (shuffle [cicadas
                                            glacier
                                            iceberg
                                            glacier1
                                            dolphins
                                            whales])
                                  (converge {:durs (flatten (repeat 14 [1 4 3 5 6 7 2]))
                                             :tempos (map #(/ % 7) (range 7 15))
                                             :cps [70]})))
      nature-c)

  (stop)
  
  (do (def human-nature-c (sample-canon (metronome 120) 
                                        human-nature
                                        (converge {:durs (flatten (repeat 14 [1 4 3 5 6 7 2]))
                                                   :tempos (map #(/ % 7) (range 7 15))
                                                   :cps [70]})))
      human-nature-c)

  (do (def all-c (sample-canon (metronome 320) 
                               (shuffle (concat human nature)) 
                               (converge {:durs (flatten (repeat 20 [1 4 3 5 6 7 2]))
                                          :tempos (map #(/ % 7) (range 7 15))
                                          :cps [70]})))
      all-c))  

;(recording-start "~/Desktop/whales-canon.wav")
;(recording-stop)

