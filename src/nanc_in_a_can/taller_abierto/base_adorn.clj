(ns nanc-in-a-can.taller-abierto.base-adorn
  (:require [nanc-in-a-can.core :refer [converge]]
            [nanc-in-a-can.standard :refer [->xos dur->sec]]
            [nanc-in-a-can.taller-abierto.instruments :as i]
            [nanc-in-a-can.taller-abierto.orbitales
             :refer
             [early-g play-sample* sample-canon]]
            [overtone.core
             :refer
             [defsynth metro-bpm metronome out pan2 play-buf:ar]]))

(comment
  (require '[overtone.core :as o :refer [sample-player]])
  (sample-player i/a1)
  (o/stop-all))


(defsynth sbase
  [sample i/silence
   n-channels 1]
  (as-> sample sig
    (play-buf:ar (:value n-channels) sig)
    (pan2 sig 0)
    (out 0 sig)))





(def canon (converge {:durs (flatten (repeat 300 [1/4 1]))
                      :tempos (map #(/ % 7) (range 7 15))
                      :cps [150]}))
(def m-rand2* (memoize rand))
(defn rate [vals index] (rand-nth
                         [1
                          (+ 1.2
                             (* 0.2
                                (rand-nth [1 -1])
                                (m-rand2* (:tempo-index vals))))]))

(defn dur [vals nome]
  (* 0.1 (dur->sec (:dur vals) (metro-bpm nome))))

(defn synth* [& {:keys [vals metronome index start-pos sample]}]
  (play-sample* [:tail early-g]
                sample
                :dur (dur vals metronome)
                :rate (rate vals index)
                :out* 0
                :a (+ 0 (rand 2))
                :r (+ 0.7 (rand 0.5))
                :bp-freq (+ 20 (rand-int 1600))
                :bp-q (max 0.1 (rand 0.5))
                :start-pos start-pos))

(def node {:instruments [i/rebotes i/a1 i/a2]
           :synth #'synth*})

(def xos (->xos "xooooooo"))

(def state (atom {:history [#'node]
                  :xos #'xos}))

(comment
  (defn adorned-sample
    [state base-sample canon-config]
    (let [period (:duration base-sample)
          bpm 60
          canon (converge (merge
                           canon-config
                           {:period period :bpm bpm}))]
      {:sbase  (sbase base-sample (:n-channels base-sample))
       :canon (sample-canon (metronome bpm)
                            state
                            canon)}
      )
    )
  (adorned-sample state
                  i/a1
                  {:durs (flatten (repeat 300 [1/4 1]))
                   :tempos (map #(/ % 7) (range 7 15))
                   :cps [150]})
  nil)
