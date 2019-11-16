(ns nanc-in-a-can.dynacan)

;; given a list of durs and a state current-index, current-elapsed calculate next n events {:dur :elapsed}
;; given the state of voice v1 calculate a cp for voice v2 and the first event after current time


;; base data
(def durs [1 2 1])
(def v1 (atom {:ratio 1}))
;;;; given a list of durs and a ratio calculate event (elapsed-at) at index i for voice v1

(defn get-event-at
  "Returns a map with the duration of the event and the time elapsed (0 based)
  at which the event will happen"
  [ratio durs index]
  ;; TODO allow start index to be something other than 0, and add initial elapsed
  (let [durs-size (count durs)
        modulo (mod index durs-size)
        completed-cycles (quot index durs-size)
        last-durs  (subvec durs 0 modulo) ;; completed durs in last cycle
        last-elapsed (apply + last-durs)  ;; elapsed in last cycle
        current-dur  (nth durs modulo)
        elapsed-in-completed (if-not (> completed-cycles 0)
                               0
                               (* completed-cycles
                                  (apply + durs)))]
    {:current-dur (* ratio current-dur)
     :elapsed (* ratio (+ elapsed-in-completed last-elapsed))}))

;;;; given a list of durs, a ratio, an event at index cp and a moment in time calculate most inmediate possible event (the one closest to elapsed-at >= 0
(do)
(defn get-partial-cycle-durs
  "start-index must already be in modulo form"
  [dur-acc ratio durs start-index max-elapsed]
  (user/spy :duracc dur-acc)
  (loop [dur-acc* dur-acc
         i (dec start-index)]
    (user/spy :durac1 dur-acc*)
    (let [next-dur (* ratio (nth durs i 0))
          next-acc-val (+ dur-acc* next-dur)]
      (if (or (< i 0)
              (> next-acc-val max-elapsed))
        (do (user/spy :durac dur-acc*)
            {:index (inc i)
             :elapsed dur-acc*
             :elapsed-at (user/spy :ea max-elapsed dur-acc* (- max-elapsed dur-acc*))
             :completed-all (> next-acc-val max-elapsed) })
        (recur next-acc-val (dec i))))))

(defn find-first-event-using-cp
  "The first event can be deduced by conceptualizing that the first event may be found by going
  backwards in time starting from the cp.
  Th cp is at some point of the durs vector (cycle) so we need to start from there and go back to the begining of the vector.
  First comes a partial cycle (pcy-end), which may be from 0 to the nth index of the nth length durs vector.
  Then there may be any number of full cycles. We do not need to add each of the durs from the cycles but rather we add the whole durs block (cyn..cy0, written as cyn->cy0).
  Finally there may be another partial cycle (pcy-start) --conceptually equal to pcy-end.
  cyn->cn0 and pcy-start will be 0 if the cp is close enough (i.e. within the reach of pcy-end.
  Thus we can calculate the first event position as follows:
  (- cp-elapsed-at (+ pcy-end cyn->cy0 pcy-start))
  The difference of cp-elapsed-at with the previous result can not be smaller than 0.
  The value that is returned is the dur index and the elapsed-at of the first event.
  "
  [ratio durs cp cp-elapsed-at]
  (let [durs-size (count durs)
        pcy-end (user/spy :pcy-end (get-partial-cycle-durs 0 ratio durs (mod cp durs-size) cp-elapsed-at))
        cycle-total (user/spy :mute :cycle-total (* ratio (apply + durs)))
        cyn->cy0 (user/spy :mute (quot (pcy-end :elapsed-at) cycle-total))
        pcy-end+cyn->cy0 (+ (pcy-end :elapsed) (* cyn->cy0 cycle-total))
        pcy-start (user/spy :start (get-partial-cycle-durs
                                    (user/spy :pcy-end-acc pcy-end+cyn->cy0)
                                    ratio
                                    durs
                                    durs-size
                                    cp-elapsed-at
                                       ))
        index (mod (pcy-start :index) durs-size)]
    {:elapsed-at (if (pcy-end :completed-all)
                   (pcy-end :elapsed-at)
                   (pcy-start :elapsed-at))
     :index index
     ;; :real-index (cond
     ;;               ;; if no full cycles will elapse, transpose index to the correct cycle of given by `cp-elapsed-at`
     ;;               (= 0 cyn->cy0) (-> (quot cp durs-size)
     ;;                                  (* durs-size)
     ;;                                  (+ index))
     ;;               ;; if many full cycles, at which index should it really start (negative index) so that the indexes of both voices coincide at the same `elapsed-at`
     ;;               (> cyn->cy0 0) (-> cyn->cy0
     ;;                                  (* durs-size)
     ;;                                  (+ (:index pcy-end))
     ;;                                  -
     ;;                                  (+ cp))
     ;;               :default index)
     }))
  ;; (find-first-event-using-cp 8/7 durs 7 7)
  


(defn get-next-event [voice durs]
  (merge voice {:index (inc (:index voice))
                :dur (* (voice :ratio)
                        (nth durs (mod (voice :index) (count durs))))
                :elapsed (+ (get voice :dur 0)
                            (get voice
                                 :elapsed
                                 (:elapsed-at voice)))}))




;;;;

(defn get-next-n-events [durs voice n]
  (loop [n* n
         index (:index voice)
         res []]
    (if (= -1 n*)
      res
      (recur (dec n*)
             (inc index)
             (conj res
                   (merge voice
                          {:index index
                           :dur (* (voice :ratio)
                                   (nth durs (mod index (count durs))))
                           :elapsed (+ (get (last res) :dur 0)
                                       (get (last res)
                                            :elapsed
                                            (:elapsed-at voice)))}))))))

(comment
  ;;test
  (user/spy (get-next-n-events [1 1 1 1]
                               (atom {:elapsed-at 0 :index 0 :ratio 1})
                               3)))

;;;;;;;;
;;Test;;
;;;;;;;;





(comment
  (require '[nanc-in-a-can.sequencing-2 :refer [schedule!]]
           '[overtone.music.time :refer [now apply-at]])
  (do
    (def now* (+ 1000 (now)))
    (reset! v1'
            (merge
             {:ratio reference-ratio}
             (find-first-event-using-cp reference-ratio
                                        durs
                                        cp
                                        elapsed-time-at-cp)
             {:fn #(println "v1" (select-keys % [:index :elapsed-at]))
              :started-at now*
              :next-event 0
              :durs durs
              :tempo 2000
              :loop? true
              :playing? true}))


    (reset! v2'
            (merge
             {:ratio subordinate-ratio}
             (find-first-event-using-cp subordinate-ratio
                                        durs
                                        cp
                                        elapsed-time-at-cp)
             {:fn #(println "v2" (select-keys % [:index :elapsed-at]))
              :started-at now*
              :next-event 0
              :durs durs
              :tempo 2000
              :loop? true
              :playing? true}))

    (println @v1')
    (println @v2')
                                        ;(schedule! v1')
                                        ; (schedule! v2')
    )

  (swap! v1' #(assoc % :loop? false))
  (swap! v2' #(assoc % :loop? false)))




;; Demonstrate that the event at `cp` in two different voices occurs at the same time
;; each voices is a map in an atom with keys: `:ratio` `:elapsed-at` `:index`
;; `elapsed-at` and `:index` for both voices are calculated with
;; `find-first-event-using-cp` and `get-event-at`.
;; IMPORTANT: Note that `get-event-at` always uses the `reference-ratio`.
;; This should be the same for all voices in the canon.

;; The function `get-next-n-events` is used as means for the demonstration,
;; see comment with example use.

;; To Do: convert into a proper test


(comment)
(defn normalize-dur [{:keys [dur ratio]}] (* dur (/ 1 ratio)))

(do
  (def cp 11)
  (def durs [1 10 1/2])
  (def reference-ratio 1)
  (def subordinate-ratio 3/11)
  (def elapsed-time-at-cp (:elapsed
                           (get-event-at reference-ratio
                                         durs
                                         cp)))
  (def main-voice (merge {:ratio reference-ratio}
                         (find-first-event-using-cp reference-ratio
                                                    durs
                                                    cp
                                                    elapsed-time-at-cp)))

  (def secondary-voice (merge {:ratio subordinate-ratio}
                              (find-first-event-using-cp subordinate-ratio
                                                         durs
                                                         cp
                                                         elapsed-time-at-cp)))
  
  ;; just print both voices
  (println "Voices data:")
  (println main-voice)
  (println secondary-voice)

  (let [mv-event (last (get-next-n-events durs main-voice cp))
        mv-elapsed-at-cp (:elapsed mv-event)]
    (loop [sv-event (get-next-event secondary-voice durs)]
      (let [sv-elapsed (get sv-event :elapsed 0)
            both-same-duration (= (normalize-dur mv-event)
                                  (normalize-dur sv-event))]
        (cond
          (and (= mv-elapsed-at-cp sv-elapsed)
               both-same-duration) (user/spy [true mv-event sv-event :same-duration])
          (= mv-elapsed-at-cp sv-elapsed) (user/spy [false mv-event sv-event :different-duration])
          (< mv-elapsed-at-cp sv-elapsed) [false mv-event sv-event]
          :else (recur (get-next-event sv-event durs))))))
)
  

