(ns nanc-in-a-can.taller-abierto.adorned-sample
  (:require [nanc-in-a-can.converge :refer [converge]]
            [nanc-in-a-can.taller-abierto.sample-canon :refer [sample-canon]]
            [nanc-in-a-can.taller-abierto.synths.sample-players :refer [sbase]]
            [overtone.core :as o]))

(defn adorned-sample
  [state base-sample canon-config chan]
  (let [period (:duration base-sample)
        bpm 60
        canon (converge (merge
                         canon-config
                         {:period period :bpm bpm}))]
    {:sbase  (sbase base-sample (:n-channels base-sample) chan)
     :canon (sample-canon (o/metronome bpm)
                          state
                          canon
                          :pan chan)}))
