(ns nanc-in-a-can.core
  (:use [overtone.live]))

(comment (
SynthDef(\sin, {|
	freq = 400,
	portamento = 0,
	portamento_amp = 0.3,
	portamento_amp_speed = 0.3,
	amp = 0.1,
	gate = 1,
	out = 0
	|
	var sig = {
		SinOsc.ar(
			Lag.kr(freq, portamento),
			0,
			Lag.kr(portamento_amp, portamento_amp_speed)
		)
		.pipe(
			// BPeakEQ.ar(_, [800, 1000, 2000, 4000], 1, -40),
			BHiShelf.ar(_, freq: 800, rs: 0.3, db: -10),
			_* EnvGen.kr(
				Env.asr(0.1, amp, 1.3),
				gate,
				doneAction:2
			),
			_*amp*0.1,
			Limiter.ar(_, level: 0.5, dur: 0.03)
		)
	} ;
	Out.ar(out, sig ! 2);
}).add;
))


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


(defn make-bcp [cp line] (sum (subvec line 0 (max 0 (- cp 1)))))

(defn make-tempo [speed] (/ 60 (/ speed 4)))

(defn total-dur [voice] (sum (map :dur (:melody voice))))

(defn make-voices [cp voices melody]
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

(make-voices
 2
 [{:tempo 60 :transp 0} {:tempo 50 :transp 7}]
 [{:dur 1 :note 60} {:dur 2 :note 62}])

(defn converge [symbol melody cp voices instruments player repeat osc meta] melody)

(make-bcp 3 [0 1 2 3 4])


(make-bcp 4 [3 6])
(apply min [1 2 3])

