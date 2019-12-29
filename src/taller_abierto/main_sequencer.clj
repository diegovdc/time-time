(ns taller-abierto.main-sequencer
  (:require [clojure.spec.alpha :as s]
            [overtone.at-at :as at-at]
            [overtone.music.time :refer [apply-at now]]
            [taller-abierto.internal :refer [arg-count validate]]))

(s/def ::synth #(= (arg-count %) 0))
(s/def ::at number?)
(s/def ::track (s/keys :req-un [::synth ::at]))
(s/def ::tracks (s/* ::track))

(defn play!
  [tracks]
  {:pre [(validate ::tracks tracks)]}
  (let [now* (now)
        player (atom [])]
    (doseq [{:keys [at synth]} tracks]
      (swap! player conj
             (apply-at (+ 1000 now* (* 1000 at))
                       synth)))
    player))


(defn stop! [player]
  (doseq [event @player] (at-at/stop event))
  :stopped)

(comment
  (def tracks [{:synth #(println 1 %) :at 0}
               {:synth #(println 2) :at 5}])
  (s/explain ::tracks tracks)
  (def player (play! tracks))
  (stop! player))
