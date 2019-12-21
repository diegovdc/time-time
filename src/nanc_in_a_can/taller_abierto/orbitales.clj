(ns nanc-in-a-can.taller-abierto.orbitales
  (:require [nanc-in-a-can.core :refer [converge]]
            [nanc-in-a-can.sequencing :refer [sequencer]]
            [nanc-in-a-can.taller-abierto.graph.core :as g]
            [nanc-in-a-can.taller-abierto.instruments :as i]
            [nanc-in-a-can.taller-abierto.standard :refer [dur->ms nthw rand-pos]]
            [overtone.core :refer :all]))

(declare xos)
(defn xo-play? [index tempo-index]
  (let [current-event (nthw xos index false)]
    (if (boolean? current-event)
      current-event
      ((set current-event) tempo-index))))

(def o1 (audio-bus 2))


(defonce main-g (group "get-on-the-bus main"))
(defonce early-g (group "early birds" :head main-g))
(defonce later-g (group "latecomers" :after early-g))
(def x true)
(def o false)

(comment
  (defsynth out100 [in* 99 amp 1] (out 0 (* amp (in in*))))
  (defonce outy (out100  :fx-group later-g)))

(declare rate)

(declare silence)
(declare play-sample*)
(def silence (freesound-sample 459659))
(defsynth play-sample*
  [smpl silence
   a 3
   r 3
   dur 0
   pan 0
   start-pos 0
   rate 1
   bp-freq 1000
   bp-q 1
   out* 0]
  (let [env (envelope [0 0.7 0.1 0] [a dur r 3] :lin)]
    (out out* (distort
               (distort
                (pan2
                 (* 5 ;; BEWARE!!!!!!!!!!!!!!!!!!!!!
                    ;; (sin-osc:kr (+ 100 (rand 30)))
                    ;; (sin-osc:kr 0.2)
                    (env-gen env :action FREE)
                    (bpf:ar
                     (play-buf:ar 2 smpl
                                  :rate rate
                                  :start-pos (min 0 (- (rand 1000) start-pos))
                                  :loop (if (= smpl silence)
                                          false
                                          true))
                     bp-freq
                     bp-q))
                 pan))))))

(defn get-instruments [state]
  (-> @state :history last var-get :instruments))
(declare dur)
(defn smpl-playa [vals index nome state sample-sequence]
  (let [at-idx (get @sample-sequence index)
        smpl (or (:smpl at-idx) (nthw (get-instruments state) index silence))
        start-pos (or (:start-pos at-idx) (rand-pos smpl))]
    (when (nil? at-idx)
      (swap! sample-sequence #(assoc % index {:start-pos start-pos
                                              :smpl smpl})))

    (when (xo-play? index (:tempo-index vals))
      (play-sample* [:tail early-g]
                    smpl
                    :dur (dur vals nome)
                    :rate (rate vals index)
                    :out* 0
                    :a (+ 0 (rand 2))
                    :r (+ 0.7 (rand 0.5))
                    :bp-freq (+ 20 (rand-int 1600))
                    :bp-q (max 0.1 (rand 0.5))
                    :start-pos start-pos))))

(defn sample-canon [nome state canon]
  (let [sample-sequence (atom {})]
    (->> canon
         (map (fn [voice]
                (sequencer
                 nome
                 voice
                 (fn [vals index]
                   (#'smpl-playa
                    vals
                    index
                    nome
                    state
                    sample-sequence))))))))



(defn semi-kill [group]
  (->> group
       (map
        (fn [atm] (stop-player (:next-event @atm))
          (swap! atm #(assoc % :stop? true))
          (:next-event @atm)))
       ))



(def m-rand (memoize (fn [_] (rand))))
(def m-rand2 (memoize rand))
(def xos [x o o x o o x o]
  #_(shuffle (concat
            (repeat 5 true)
            (repeat 1 false))))

(defn rate [vals index] (rand-nth
                         [1
                          (user/spy
                           :mute
                           (+ 0.8
                              (* 0.2
                                 (rand-nth [1 -1])
                                 (m-rand2 (:tempo-index vals)))))]))

(defn dur [vals nome]
  (* 0.1 (dur->ms (:dur vals) (metro-bpm nome))))

(def node-a {:instruments [i/orbitales]})

(def node-b {:instruments [i/rebotes]})

(def node-c {:instruments [i/a1 i/a2]})

(def graph {#'node-a #{#'node-b #'node-c}
            #'node-b #{#'node-a}
            #'node-c #{#'node-b}})

(def state (atom {:history []
                  :status :playing
                  :config {}}))

(user/spy (g/play-next! state graph))

(do
  (get-instruments state))
(comment
  (do (def nature-c
        (sample-canon (metronome 30)
                      state
                      (converge {:durs (flatten (repeat 300 [1/4 1]))
                                 :tempos (map #(/ % 7) (range 7 15))
                                 :cps [150]})))
      nature-c)

  (semi-kill nature-c)
  (stop))


(kill)

(comment
  (recording-start "C:\\Users\\Diego\\Desktop\\orbitales-canon-v2.palecs.wav")
  (recording-stop)

  (demo (sin-osc:ar 400)))
