(ns nanc-in-a-can.core
  (:use [overtone.live]))

;; (comment (
;; SynthDef(\sin, {|
;; 	freq = 400,
;; 	portamento = 0,
;; 	portamento_amp = 0.3,
;; 	portamento_amp_speed = 0.3,
;; 	amp = 0.1,
;; 	gate = 1,
;; 	out = 0
;; 	|
;; 	var sig = {
;; 		SinOsc.ar(
;; 			Lag.kr(freq, portamento),
;; 			0,
;; 			Lag.kr(portamento_amp, portamento_amp_speed)
;; 		)
;; 		.pipe(
;; 			// BPeakEQ.ar(_, [800, 1000, 2000, 4000], 1, -40),
;; 			BHiShelf.ar(_, freq: 800, rs: 0.3, db: -10),
;; 			_* EnvGen.kr(
;; 				Env.asr(0.1, amp, 1.3),
;; 				gate,
;; 				doneAction:2
;; 			),
;; 			_*amp*0.1,
;; 			Limiter.ar(_, level: 0.5, dur: 0.03)
;; 		)
;; 	} ;
;; 	Out.ar(out, sig ! 2);
;; }).add;
;; ))


(defsynth sin1 [freq 400 
               portamento 0 
               portamento-amp 0.3
               portamento-amp-speed 0.3 
               amp 0.1 
               gate 1 
               out' 0]
  (let [sig (sin-osc 
             (lag:kr freq portamento) 
             0 
             (lag:kr portamento-amp portamento-amp-speed))
        sig (b-hi-shelf sig 800 0.3 -10)
        sig (* sig amp 0.1)]
    (out out' sig)))

(sin1 440)
(stop)


;; nucleo del converge original traducido a clj
(defn make-bcp [cp line] (sum (subvec line 0 (max 0 (- cp 1)))))

(defn make-tempo [speed] (/ 60 (/ speed 4)))

(defn total-dur [voice] (sum (map :dur (:melody voice))))

(defn make-voices [{:keys [cp voices melody]}]
  (->> voices 
       (map (fn [voice] ; define each melody event for each voice 
              (->> melody 
                   (map (fn [event] 
                          {:dur (* (:dur event) (make-tempo (:tempo voice)))
                           :note (+ (:note event) (:transp voice))
                           :amp (:amp event)
                           :cp cp})))))
       (map (fn [voice] ; add bcp (before convergence point)
              (let [bcp  (make-bcp cp (mapv :dur voice))] 
                {:melody voice :bcp bcp})))
       (sort (fn [a, b] (> (total-dur a) (total-dur b)))); sort voices by duration
       ((fn [voices] ; add onset data
          (let [min-bcp  (apply min (map #(get-in % [:bcp]) voices))]
            (map #(assoc % :onset (Math/abs (float (- min-bcp (:bcp %))))) voices))))))

(user/spy (make-voices
           {:cp 2
            :voices [{:tempo 60 :transp 0} {:tempo 50 :transp 7}]
            :melody [{:dur 1 :note 60} {:dur 2 :note 62}]}))

(defn converge [symbol melody cp voices instruments player repeat osc meta] melody)

(make-bcp 3 [0 1 2 3 4])


;; (make-bcp 4 [3 6])
;; (apply min [1 2 3])


;; wip implementando converge de otra manera, con :elapsed en durs
(do
  (defn converge [{:keys [durs tempos]}]
    (let [vdurs (map (fn [tempo] 
                       (reduce (fn [acc dur]
                                 (let [new-dur (/ dur tempo)
                                       last-dur (-> acc last :dur (or 0))
                                       last-elapsed (-> acc last :elapsed (or 0))
                                       elapsed (+ last-dur last-elapsed)]
                                   (conj acc {:dur new-dur :elapsed elapsed}))
                                 ) [] durs)) 
                     tempos)
          ]
      vdurs)
    )

  ((user/capture :vdurs) (converge {:durs [1 2 3 4]
                               :tempos [1 2 3]})))

(-> []  last :elapsed (or 0))


;; transposicion temporal usando :elapsed, multiples cps
(let [vdurs (@user/data :vdurs)
      sorted-durs (->> vdurs (sort-by (comp - :elapsed last))) ;; from longest to shortest
      longest (first sorted-durs)
      cp [2 3]
      get-elapsed-at-cp (fn [cp voice] (-> voice (nth cp) :elapsed))
      cps-at-longest (mapv #(get-elapsed-at-cp % longest) cp)
      add-cp-offset (fn [index voice] 
                      (let [current-cp (cp (mod index (count cps-at-longest)))
                            current-elapsed-cp (cps-at-longest (mod index (count cps-at-longest)))
                            offset (- current-elapsed-cp (get-elapsed-at-cp current-cp voice))]
                        (map 
                         (fn [vdur] 
                           (update-in vdur [:elapsed] #(+ offset %)))
                         (user/spy :mute :voice voice))))]
  (user/spy :longest longest)
  (user/spy :shorter (map-indexed add-cp-offset (rest vdurs)))
  )
