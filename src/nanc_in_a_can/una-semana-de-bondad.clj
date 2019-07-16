(ns nanc-in-a-can.una-semana-de-bondad
  (:require [overtone.live :refer :all]
            [nanc-in-a-can.sequencing :refer [sequencer]]))

(def nome (metronome 10))

(def silence (freesound-sample 459659))

(def whales-1 (freesound-sample 322539))

(definst play-sample* [smpl silence dur 0 pan 0 start-pos 0 rate 1] 
  (let [env (envelope [0 1 0] [1 dur 1.2] :lin)]
    (pan2 (* 0.7
             (env-gen env :action FREE) 
             (play-buf:ar 1 smpl
                          :rate rate
                          :start-pos start-pos 
                          :loop (if (= smpl silence) 
                                  false 
                                  true))) 
          pan)))

(defn rand-pos [smpl] (rand-int (:n-samples smpl)))

(defn dur->ms [dur bpm] (* dur (/ 60 bpm)))



(comment (play-sample* whales-1 10)         
         (stop))




(comment
  (do
    (def sample-sequence (atom {}))

    (->> @user/data :canon
         (map (fn [voice] (sequencer 
                          nome 
                          voice
                          (fn [vals index]
                            (let [at-idx (get @sample-sequence index)
                                  smpl (or (:smpl at-idx) whales-1)
                                  start-pos (or (:start-pos at-idx) (rand-pos smpl))]
                              (when (nil? at-idx)
                                (swap! sample-sequence #(assoc % index {:start-pos start-pos
                                                                        :smpl smpl})))
                              (play-sample* smpl                            
                                            ;; dur + 2 secs for fade-in/out
                                            (+ 2 (dur->ms (:dur vals) 
                                                          (metro-bpm nome)))
                                            :rate (inc (* index 0.2))
                                            :start-pos start-pos)))))))))



(comment (stop))
  
  

;(recording-start "~/Desktop/whales-canon.wav")
;(recording-stop)

