(ns time-time.standard
  (:require
   [clojure.string :as string]))

(defn rand-pos [smpl] (rand-int (:n-samples smpl)))

(defn dur->sec
  "Takes an absolute duration and converts it to seconds"
  [dur bpm]
  (* dur (/ 60 bpm)))

(defn dur->ms
  "Takes an absolute duration and converts it to milliseconds"
  [dur bpm]
  (* dur (/ 60 bpm) 1000))

(defn nthw [coll index not-found]
  (try
    (let [i (mod index (count coll))]
      (nth coll i))
    (catch Throwable _  not-found)))

(defn wrap-at [i coll]
  (let [i* (mod i (count coll))]
    (nth coll i*)))

(comment (map #(wrap-at % [1 2]) (range 10)))

(defn ->xos [xo-string]
  (->> (string/split xo-string #"")
       (map #(= "x" %))))
