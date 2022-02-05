If a defsynth fails the error is swallowed and no message is thrown

```supercollider
(defn replay [buf-key]
    (let [b (@granular/bufs buf-key)
          subcps "1)4 of 3)6 3.9-1.5.7.11"
          scale (-> eik :subcps (get subcps) :scale)]
      (ref-rain
       :id :fl-grain
       :tempo 900
       :ratio 1/35
       :durs [8 8 8 15 8 8 8 15 8 8 15]
       :on-event (on-event
                  (let [root (weighted {2 70 1 30})
                        intervals (weighted {[1] 20
                                             [3 10] 10
                                             [0 4] 10
                                             [0 7] 10
                                             [-2] 30
                                             [-4] 30
                                             [8] 5})
                        start (rand 0.3)
                        end (min 1 (+ start (rand)))
                        dur* (max 0.05 (rand 0.5))]
                    #_(println "replay" intervals subcps)
                    (mapv
                     #(#'granular/grain b dur*
                                        :trig-rate 100
                                        :grain-dur 1/5
                                        :rate (interval->ratio scale root %)
                                        :amp (max 0.1 (rand))
                                        :start start
                                        :end end
                                        :mix 1 // This fails because there are too many args
                                        :pan (- (rand 2) 1)
                                        :a (* 0.1 dur*)
                                        :r (* 0.5 dur*))
                     intervals))))))
```
