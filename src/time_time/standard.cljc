(ns time-time.standard
  #?@
   (:clj
    [(:require [clojure.string :as string])]
    :cljs
    [(:require
      ["tone/build/esm/index" :as Tone]
      [clojure.string :as string])]))

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
    #?(:clj (catch Throwable _  not-found)
       :cljs (catch js/Error _ not-found))))

(defn wrap-at [i coll]
  (let [i* (mod i (count coll))]
    (nth coll i*)))

(comment (map #(wrap-at % [1 2]) (range 10)))

(defn ->xos [xo-string]
  (->> (string/split xo-string #"")
       (map #(= "x" %))))

(defn now
  "Returns the current time in ms"
  []
  #?(:clj (System/currentTimeMillis)
     ;; NOTE We use Tone/now instead of Date.now for ease of use (fingers crossed)
     :cljs (* 1000 (Tone/now))))

(defn rrand
  "Random number in range"
  ([bottom top]
   (let [rand-fn (if (some float? [bottom top])
                   rand rand-int)]
     (-> (- top bottom)
         rand-fn
         (+ bottom)))))

(defn rotate [coll n]
  (let [l (count coll)
        off (mod (+ (mod n l) l) l)]
    (concat (drop off coll) (take off coll))))
