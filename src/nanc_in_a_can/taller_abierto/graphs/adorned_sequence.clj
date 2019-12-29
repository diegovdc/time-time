(ns nanc-in-a-can.taller-abierto.graphs.adorned-sequence
  "Protoipo de una sequencia de canones adornados, ver `tracks`"
  (:require [nanc-in-a-can.standard :refer [->xos dur->sec]]
            [nanc-in-a-can.taller-abierto.adorned-sample :refer [adorned-sample]]
            [nanc-in-a-can.taller-abierto.instruments :as i]
            [nanc-in-a-can.taller-abierto.standard :refer [ch]]
            [nanc-in-a-can.taller-abierto.synths.sample-players :refer [m-distort]]
            [overtone.core :as o]))

(def m-rand2* (memoize rand))

(defn synth* [& {:keys [vals metronome index start-pos sample pan]}]
  (m-distort sample
             :dur (* 0.1 (dur->sec (:dur vals) (o/metro-bpm metronome)))
             :rate (rand-nth
                    [1
                     (+ 1.2
                        (* 0.2
                           (rand-nth [1 -1])
                           (m-rand2* (:tempo-index vals))))])
             :out* 0
             :a (+ 0 (rand 2))
             :r (+ 0.7 (rand 0.5))
             :bp-freq (+ 20 (rand-int 1600))
             :bp-q (max 0.1 (rand 0.5))
             :start-pos start-pos
             :pan pan))

(def canon-config {:durs (flatten (repeat 300 [1/4 1]))
                   :tempos (map #(/ % 7) (range 7 20))
                   :cps [150 300 450 590]})

;; 1
(def n1 {:instruments [i/rebotes i/a1 i/a2] :synth #'synth*})
(def xo1 (->xos "xoooo"))
(def s1 (atom {:history [#'n1] :xos #'xo1}))
;; 2
(def n2 {:instruments [i/a2 i/a2] :synth #'synth*})
(def xo2 (->xos "xoooo"))
(def s2 (atom {:history [#'n2] :xos #'xo2}))
;; 3
(def n3 {:instruments [i/a3 i/a3] :synth #'synth*})
(def xo3 (->xos "xoooo"))
(def s3 (atom {:history [#'n3] :xos #'xo3}))
;; 4
(def n4 {:instruments [i/a4 i/a4] :synth #'synth*})
(def xo4 (->xos "xoooo"))
(def s4 (atom {:history [#'n4] :xos #'xo4}))

(def tracks [{:at 0
              :synth #(adorned-sample s1 i/a1 canon-config (ch 1))}
             {:at 0
              :synth #(adorned-sample s2 i/a8 canon-config (ch 2))}
             {:at 15
              :synth #(adorned-sample s3 i/orb2 canon-config (ch 3)) }
             {:at 20
              :synth #(adorned-sample s4 i/orb1 canon-config (ch 4))}])
(comment
  (require '[nanc-in-a-can.taller-abierto.main-sequencer :as ms])
  (def player (ms/play! tracks))
  (ms/stop! player))
