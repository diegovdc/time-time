(ns nanc-in-a-can.dynacan)

;; given a list of durs and a state current-index, current-elapsed calculate next n events {:dur :elapsed}
;; given the state of voice v1 calculate a cp for voice v2 and the first event after current time


;; base data
(def durs [1 2 1])
(def v1 (atom {:ratio 1}))

;;;; given a list of durs and a ratio calculate event (elapsed-at) at index i for voice v1
(do
  (defn get-event-at [ratio durs index]
    ;; TODO allow start index to be something other than 0, and add initial elapsed
    (let [durs-size (count durs)
          modulo (mod index durs-size)
          completed-cycles (quot index durs-size)
          last-durs (user/spy :last-durs (subvec durs 0 modulo)) ;; completed durs in last cycle
          last-elapsed (apply + last-durs) ;; elapsed in last cycle
          current-dur (user/spy (nth durs modulo))
          elapsed-in-completed (if-not (> completed-cycles 0)
                                 0
                                 (* completed-cycles 
                                    (apply + durs)))
          ]
      {:current-dur (* ratio current-dur)
       :elapsed (* ratio (+ elapsed-in-completed last-elapsed))}))
  (get-event-at 0.5 durs 7))


;;;; given a list of durs, a ratio, an event at index cp and a moment in time calculate most inmediate possible event (the one closest to elapsed-at >= 0
(do
  (defn get-partial-cycle-durs 
    "start-index must already be in modulo form"
    [dur-acc ratio durs start-index max-elapsed]
    (loop [dur-acc* dur-acc
           i (dec start-index)]
      (let [next-dur (* ratio (nth durs i 0))
            next-acc-val (+ dur-acc* next-dur)]
        (if (or (< i 0)
                (> next-acc-val max-elapsed)) 
          {:index (inc i) :elapsed dur-acc* :elapsed-at (- max-elapsed dur-acc*)}
          (recur next-acc-val (dec i))))
      ))
  
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
          pcy-end (get-partial-cycle-durs 0 ratio durs (mod cp durs-size) cp-elapsed-at)
          cycle-total (* ratio (apply + durs))
          cyn->cy0 (quot (pcy-end :elapsed-at) cycle-total)
          pcy-end+cyn->cy0 (+ (pcy-end :elapsed) (* cyn->cy0 cycle-total))
          pcy-start (get-partial-cycle-durs pcy-end+cyn->cy0
                                            ratio
                                            durs
                                            durs-size
                                            cp-elapsed-at)]
      {:elapsed-at (pcy-start :elapsed-at)
       :index (mod (pcy-start :index) durs-size)}
      )
    )
  (find-first-event-using-cp 8/7 durs 7 9))


;;;; given a voice v1' with stateful index, get next event from durs

(def v1' (atom {:index 0
                :elapsed-at 0
                :ratio 1}))

(def v2' (atom {:index 2
                :elapsed-at 15/7
                :ratio 8/7}))

(defn get-next-event [durs voice]
  {:index (inc (:index @voice))
   :elapsed-at (+ (:elapsed-at @voice) 
                  (nth durs (mod (:index @voice) (count durs))))})

(swap! v1' #(merge % (get-next-event durs v1')))


;;;; 
(do
  (defn get-next-n-events [durs voice n]
    (loop [n* n
           index (:index @voice)
           res []]
      (if (= -1 n*)
        res
        (recur (dec n*)
               (inc index)
               (conj res {:index index
                          :dur (* (@voice :ratio) 
                                  (nth durs (mod index (count durs))))
                          :elapsed (+ (get (last res) :dur 0) 
                                      (get (last res) 
                                           :elapsed 
                                           (:elapsed-at @voice)))}))))
    )
  ;; nota: el indice 7 es el cp
  (->> [v1' v2']
       (map #(as-> % v 
               (get-next-n-events durs v 7)
               (filterv (fn [ev] (= 7 (:index ev))) v)
               (map :elapsed v)
               )
            )
       flatten
       (apply =)) ;; ambas voces tienen el mismo valor de elapsed-at en el punto de convergencia?
  )



(require '[overtone.music.time :refer :all])

(apply-at (+ 1000 (now)) #(println "hola"))
