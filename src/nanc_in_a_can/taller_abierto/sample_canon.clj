(ns nanc-in-a-can.taller-abierto.sample-canon
  (:require [nanc-in-a-can.sequencing :refer [sequencer]]
            [nanc-in-a-can.standard :refer [nthw rand-pos]]
            [nanc-in-a-can.taller-abierto.standard :refer [get-instruments get-synth xo-play?]]
            [nanc-in-a-can.taller-abierto.instruments :as i]))

(defn smpl-playa [vals index nome state sample-sequence pan]
  (let [at-idx (get @sample-sequence index)
        smpl (or (:smpl at-idx) (nthw (get-instruments state) index i/silence))
        start-pos (or (:start-pos at-idx) (rand-pos smpl))
        synth* (get-synth state)]
    (when (nil? at-idx)
      (swap! sample-sequence #(assoc % index {:start-pos start-pos
                                              :smpl smpl})))

    (when (xo-play? state index)
      (synth* :vals vals
              :metronome nome
              :index index
              :sample smpl
              :start-pos start-pos
              :pan pan))))

(defn sample-canon [nome state canon & {:keys [pan] :or {pan 0}}]
  (let [sample-sequence (atom {})]
    (->> canon
         (mapv (fn [voice]
                (sequencer
                 nome
                 voice
                 (fn [vals index]
                   (#'smpl-playa
                    vals
                    index
                    nome
                    state
                    sample-sequence
                    pan))))))))
