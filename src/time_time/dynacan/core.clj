(ns time-time.dynacan.core
  (:require [clojure.spec.alpha :as spec]
            [time-time.utils.core :refer [rotate]]))

(spec/def ::index int?)
(spec/def ::ratio rational?)
(spec/def ::elapsed number?)
(spec/def ::echoic-distance number?)
(spec/def ::echoic-distance-event-qty int?)
(spec/def ::voice (spec/keys :req-un [::index ::ratio ::elapsed]
                             :opt-un [::echoic-distance-event-qty
                                      ::echoic-distance]))


(defn get-event-at
  "Returns a map with the duration of the event and the time elapsed (0 based)
  at which the event will happen"
  ([ratio durs index start-index]
   (get-event-at ratio (into [] (rotate durs start-index)) index))
  ([ratio durs index]
   (let [durs-size (count durs)
         modulo (mod index durs-size)
         completed-cycles (quot index durs-size)
         last-durs  (subvec durs 0 modulo) ;; completed durs in last cycle
         last-elapsed (apply + last-durs)  ;; elapsed in last cycle
         current-dur (nth durs modulo)
         elapsed-in-completed (if-not (> completed-cycles 0)
                                0
                                (* completed-cycles
                                   (apply + durs)))]
     {:current-dur (* ratio current-dur)
      :elapsed (* ratio (+ elapsed-in-completed last-elapsed))})))

;;;; given a list of durs, a ratio, an event at index cp and a moment in time calculate most inmediate possible event (the one closest to elapsed-at >= 0

(defn get-partial-cycle-durs
  ;; NOTE start-index must already be in modulo form
  [dur-acc ratio durs start-index max-elapsed]
  (loop [dur-acc* dur-acc
         i (dec start-index)]
    (let [next-dur (* ratio (nth durs i 0))
          next-acc-val (+ dur-acc* next-dur)]
      (if (or (< i 0)
              (> next-acc-val max-elapsed))
        {:index (inc i)
         :elapsed dur-acc*
         :elapsed-at (- max-elapsed dur-acc*)
         :completed-all (> next-acc-val max-elapsed) }
        (recur next-acc-val (dec i))))))
;;*

(do
  (defn find-first-event-using-cp
    "The first event can be deduced by conceptualizing that the first event may
  be found by going backwards in time starting from the cp.
  Th cp is at some point of the durs vector (cycle) so we need to start from
  there and go back to the begining of the vector.
  First comes a partial cycle (`pcy-end`), which may be from 0 to the nth index of the `durs` vector.
  Then there may be any number of full cycles. We do not need to add each of the
  durs from the cycles but rather we add the whole durs block (cyn..cy0, written
  as `cyn->cy0`).
  Finally there may be another partial cycle (`pcy-start`) --conceptually equal to
  `pcy-end`.
  `cyn->cn0` and `pcy-start` will be 0 if the cp is close enough (i.e. within the
  reach of `pcy-end`.
  Thus we can calculate the first event position as follows:
  (- cp-elapsed-at (+ pcy-end cyn->cy0 pcy-start))
  The difference of cp-elapsed-at with the previous result can not be smaller
  than 0.
  The value that is returned is the dur `index` and the `elapsed-at` of the first
  event.

  Options:
  `:loop?` Whether the sequence can start at a point prior to `index` 0, which means that the sequence needs to loop to reach the `cp`"
    [ratio durs cp cp-elapsed-at & {:keys [loop? start-index]
                                    :or {loop? true
                                         start-index 0}}]
    (let [durs-size (count durs)
          pcy-end (get-partial-cycle-durs
                   0
                   ratio
                   durs
                   (mod cp durs-size)
                   cp-elapsed-at)
          cycle-total (* ratio (apply + durs))
          cyn->cy0 (quot (pcy-end :elapsed-at) cycle-total)
          pcy-end+cyn->cy0 (+ (pcy-end :elapsed) (* cyn->cy0 cycle-total))
          pcy-start (get-partial-cycle-durs
                     pcy-end+cyn->cy0
                     ratio
                     durs
                     durs-size
                     cp-elapsed-at)
          loop-cycle? (and loop? (false? (pcy-end :completed-all)) )
          index (if loop-cycle?
                  (pcy-start :index)
                  (pcy-end :index))
          elapsed-at (if loop-cycle?
                       (pcy-start :elapsed-at)
                       (pcy-end :elapsed-at))
          echoic-distance (- cp-elapsed-at elapsed-at)
          start-index (mod index durs-size)]
      {:ratio ratio
       :elapsed elapsed-at
       :index start-index
       ;; :durs durs
       :cp cp
       :cp-at cp-elapsed-at
       :echoic-distance echoic-distance
       :echoic-distance-event-qty
       ;; #dbg
       (cond
         (= echoic-distance 0) 0
         (not loop-cycle?) (- (mod cp durs-size) start-index)
         :else (+
                ;; start-index of the end cycle
                (mod cp durs-size)
                ;; intermediate cycles
                (* durs-size cyn->cy0)
                ;; start index of last cycle
                (- durs-size (pcy-start :index))))}))

  (comment (find-first-event-using-cp 1/2 [1 2 3] 2 3)
           (find-first-event-using-cp 1 [1 1 1] 2 1)))

(defn normalize-dur [{:keys [dur ratio]}]
  (* dur (/ 1 ratio)))

(defn modulo-cp
    "Finds the `cp` in modulo form for given the
  `current-index` and the `echoic-distance-event-qty`"
    [ref-durs ref-current-index echoic-distance-event-qty]
    (mod (+ ref-current-index echoic-distance-event-qty)
         (count ref-durs)))

(defn ensure-rel-voice-cp
    "Makes sure that the relative voice has a definite cp, specially if `rel-voice-cp`
  is not defined"
    [current-ref-index ref-durs echoic-distance-qty rel-voice-durs rel-voice-cp]
    (if (and (= ref-durs rel-voice-durs) (nil? rel-voice-cp))
      (modulo-cp ref-durs current-ref-index echoic-distance-qty)
      (or rel-voice-cp echoic-distance-qty)))

(defn find-relative-voice-first-event
  "Calculates the first event at which the new voice will start in order for it
  to converge at `cp`.
  NOTE: It is assumed that the `ref-voice` will provide a current index which
  should reflect it's actual state (i.e. it may come from an atom or some sort of state)
  NOTE: `cp` may be `nil` and then `ensure-rel-voice-cp` will find a default value."
  [echoic-distance-event-qty
   ref-voice
   {:keys [durs cp ratio loop?] :or {loop? true} :as rel-voice}]
  (let [current-ref-index (:index ref-voice)
        ref-durs (:durs ref-voice)
        rel-voice-cp (ensure-rel-voice-cp current-ref-index ref-durs echoic-distance-event-qty durs cp)
        ref-start-index (mod current-ref-index (count ref-durs))
        cp-event (get-event-at (:ratio ref-voice)
                               ref-durs
                               echoic-distance-event-qty
                               ref-start-index)]
    (find-first-event-using-cp ratio
                               durs
                               rel-voice-cp
                               (:elapsed cp-event)
                               :loop? loop?)))
