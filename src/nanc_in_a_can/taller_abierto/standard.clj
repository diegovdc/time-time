(ns nanc-in-a-can.taller-abierto.standard
  (:require [overtone.core :refer :all]))

(defn rand-pos [smpl] (rand-int (:n-samples smpl)))

(defn dur->ms [dur bpm] (* dur (/ 60 bpm)))

(defn nthw [coll index not-found]
  (try
    (let [i (mod index (count coll))]
      (nth coll i))
    (catch Throwable _  not-found)))
