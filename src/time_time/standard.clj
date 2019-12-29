(ns time-time.standard
  (:require
   [clojure.string :as string]
   [overtone.core :refer :all]))

(defn rand-pos [smpl] (rand-int (:n-samples smpl)))

(defn dur->sec
  "Takes an absolute duration and converts it to seconds"
  [dur bpm]
  (* dur (/ 60 bpm)))

(defn nthw [coll index not-found]
  (try
    (let [i (mod index (count coll))]
      (nth coll i))
    (catch Throwable _  not-found)))

(defn ->xos [xo-string]
  (->> (string/split xo-string #"")
       (map #(= "x" %))))
