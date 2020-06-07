(ns time-time.dynacan)

;; given a list of durs and a state current-index, current-elapsed calculate next n events {:dur :elapsed}
;; given the state of voice v1 calculate a cp for voice v2 and the first event after current time


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
(defn find-first-event-using-cp
  "The first event can be deduced by conceptualizing that the first event may
  be found by going backwards in time starting from the cp.
  Th cp is at some point of the durs vector (cycle) so we need to start from
  there and go back to the begining of the vector.
  First comes a partial cycle (pcy-end), which may be from 0 to the nth index of
  the nth length durs vector.
  Then there may be any number of full cycles. We do not need to add each of the
  durs from the cycles but rather we add the whole durs block (cyn..cy0, written
  as cyn->cy0).
  Finally there may be another partial cycle (pcy-start) --conceptually equal to
  pcy-end.
  cyn->cn0 and pcy-start will be 0 if the cp is close enough (i.e. within the
  reach of pcy-end.
  Thus we can calculate the first event position as follows:
  (- cp-elapsed-at (+ pcy-end cyn->cy0 pcy-start))
  The difference of cp-elapsed-at with the previous result can not be smaller
  than 0.
  The value that is returned is the dur `index` and the `elapsed-at` of the first
  event."
  [ratio durs cp cp-elapsed-at]
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
        index (if (pcy-end :completed-all)
                (pcy-end :index)
                (pcy-start :index))
        elapsed-at (if (pcy-end :completed-all)
                     (pcy-end :elapsed-at)
                     (pcy-start :elapsed-at))]
    {:ratio ratio
     :elapsed elapsed-at
     :index (mod index durs-size)
     :cp cp
     :cp-at cp-elapsed-at
     :echoic-distance (- cp-elapsed-at elapsed-at)
     :echoic-distance-event-qty  (+
                                  ;; start-index of the end cycle
                                  (mod cp durs-size)
                                  ;; intermediate cycles
                                  (* durs-size cyn->cy0)
                                  ;; start index of last cycle
                                  (- durs-size (pcy-start :index)))}))

(defn get-next-event [voice durs]
  (merge voice {:index (inc (:index voice))
                :dur (* (voice :ratio)
                        (nth durs (mod (voice :index) (count durs))))
                :elapsed (+ (get voice :dur 0)
                            (get voice
                                 :elapsed
                                 (:elapsed voice)))}))



(defn normalize-dur [{:keys [dur ratio]}]
  (* dur (/ 1 ratio)))


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
                   (merge
                    voice
                    (let [original-dur (nth durs (mod index (count durs)))
                          dur (* (voice :ratio) original-dur)]
                      {:index index
                       :dur dur
                       :original-dur original-dur
                       :elapsed (+ (get (last res) :dur 0)
                                   (get (last res)
                                        :elapsed
                                        (:elapsed voice)))
                       :echoic-distance (if (empty? res)
                                          (voice :echoic-distance)
                                          (- (-> res
                                                 last
                                                 :echoic-distance)
                                             (-> res last :dur)))})))))))


(comment
  ;;test
  (get-next-n-events
   [1 1 1 2]
   {:echoic-distance 4 :elapsed 0 :index 0 :ratio 1}
   3))










;;;;;;;;
;;Test;;
;;;;;;;;

(comment
  (require '[clojure.test :refer [deftest testing is]]))


(comment
  (require '[time-time.sequencing-3 :refer [play!]]
           '[overtone.music.time :refer [now apply-at]]
           '[taoensso.timbre :as log])
  (let [durs [2 2 4 2]
        reference-ratio 1
        subordinate-ratio 2/3
        cp 2
        cp-elapsed-at (:elapsed (get-event-at reference-ratio durs cp))
        now* (+ 1000 (now))
        subordinate-first-event (user/spy (find-first-event-using-cp
                                           subordinate-ratio
                                           durs
                                           cp
                                           cp-elapsed-at))
        log-fn (fn [id]
                 (fn [{:keys [data]}] (log/info id (data :index) (- (now) now*))))]
    (do
      (def v1' (play! durs (log-fn "v1") :ratio reference-ratio :start-time now*))
      (def v2' (play! durs (log-fn "v2")
                      :ratio subordinate-ratio
                      :start-time now*
                      :start-index (:index subordinate-first-event)
                      :elapsed (:elapsed subordinate-first-event)
                      :loop? true))))

  (swap! v1' #(assoc % :loop? false))
  (swap! v2' #(assoc % :playing? false)))





(require '[clojure.spec.alpha :as s]
         '[clojure.spec.test.alpha :as stest]
         '[clojure.spec.gen.alpha :as gen]
         )

(s/def ::ratio (s/and rational? #(> % 0)))
(s/def ::durs (s/coll-of ::ratio))
(s/def ::index (s/and int? #(> % 0)))
(s/def ::elapsed-at ::ratio)
(s/def ::elapsed ::ratio)
(s/def ::cp ::index)
(gen/generate (s/gen ::ratio))

(s/fdef find-first-event-using-cp
  :args (s/cat :ratio ::ratio
               :durs ::durs
               :cp ::cp
               :cp-elapsed-at ::ratio)
  :ret (s/keys ::elapsed ::index))

(gen/generate (s/gen (s/cat :ratio ::ratio
                            :durs ::durs
                            :cp ::cp
                            :cp-elapsed-at ::ratio)))

(stest/instrument `find-first-event-using-cp)
