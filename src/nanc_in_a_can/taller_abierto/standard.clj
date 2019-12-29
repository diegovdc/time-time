(ns nanc-in-a-can.taller-abierto.standard
  (:require [overtone.core :refer :all]))

(def ^:dynamic *out-channels* 2)

(def ch
  "The different channels for a pan-az ugen"
  (case *out-channels*
    2 {1 0.25
       2 0.75}
    4 {1 -0.25
       2 0.25
       3 0.75
       4 1.25}))

(defn get-instruments [state]
  (-> @state :history last var-get :instruments))

(defn get-synth [state]
  (-> @state :history last var-get :synth))

(defn xo-play? [state index]
  (let [xos (var-get (@state :xos))
        len (count xos)]
    (nth xos (mod index len))))
