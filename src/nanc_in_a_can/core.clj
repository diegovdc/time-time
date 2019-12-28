(ns nanc-in-a-can.core
  (:require [nanc-in-a-can.standard :refer [dur->sec]]))

;; transposicion temporal usando :elapsed, multiples cps
(defn- converge-transposition [durs cps]
  (let [vdurs durs
        cp (if (vector? cps) cps [cps])
        sorted-durs (->> vdurs (sort-by (comp - :elapsed last))) ;; from longest to shortest
        longest (first sorted-durs)
        total-dur (->> (last longest) vals (apply +))
        get-elapsed-at-cp (fn [cp voice] (-> voice (nth cp) :elapsed))
        cps-at-longest (mapv #(get-elapsed-at-cp % longest) cp)
        add-cp-offset (fn [index voice]
                        (let [current-cp (cp (mod index (count cps-at-longest)))
                              current-elapsed-cp (cps-at-longest
                                                  (mod index
                                                       (count cps-at-longest)))
                              offset (- current-elapsed-cp
                                        (get-elapsed-at-cp current-cp voice))]
                          (mapv
                           (fn [vdur]
                             (update-in vdur [:elapsed] #(+ offset %)))
                           (user/spy :mute :voice voice))))
        add-remainder (fn [voice] (let [remainder (->> (last voice)
                                                       vals
                                                       (apply +)
                                                       (- total-dur))
                                        last-event {:dur remainder
                                                    :elapsed (+ (:dur (last voice))
                                                                (:elapsed (last voice)))
                                                    :remainder? true}]
                                    (conj voice last-event)))
        offseted-voices (map-indexed add-cp-offset (rest vdurs))]
    (map add-remainder (conj offseted-voices longest))))

(defn get-period-scaling-factor
  "Returns a factor to rescale tempos to fit make a canon fit a given period"
  [period total-dur bpm]
  (->> (dur->sec total-dur bpm)
       (/ period)))

(defn converge [{:keys [durs tempos cps period bpm] :or {cps [0]}}]
  "Optional params:
    :period - in seconds
    :bpm - used for setting period, and required because of that"
  (let [slowest-tempo (apply min tempos)
        total-dur (->> durs
                       (map #(* slowest-tempo %))
                       (apply +))
        scaling-factor (if-not (and period bpm)
                         1
                         (get-period-scaling-factor
                          period
                          total-dur
                          bpm))
        vdurs (map-indexed
               (fn [index tempo]
                 (reduce (fn [acc dur]
                           (let [new-dur (* scaling-factor (/ dur tempo))
                                 last-dur (-> acc last :dur (or 0))
                                 last-elapsed (-> acc last :elapsed (or 0))
                                 elapsed (+ last-dur last-elapsed)]
                             (conj acc
                                   {:dur new-dur
                                    :elapsed elapsed
                                    :tempo tempo
                                    :tempo-index index})))
                         []
                         durs))
               tempos)
        canon (converge-transposition vdurs cps)]
    (with-meta canon {:total-dur total-dur
                      :bpm (and period bpm)
                      :period (and bpm period)
                      :scaled-dur (* total-dur scaling-factor)
                      :scaled? (not= 1 scaling-factor)
                      :scaling-factor scaling-factor})))


(defn canon-dur
  "Returns the total duration of the canon in seconds"
  [canon bpm]
  (-> canon meta :total-dur (dur->sec bpm)))

(do
  ;;;  test `get-period-scaling-factor`
  (let [durs [1 1 1 1 1]
        slowest-tempo 1
        total-dur (* slowest-tempo (apply + durs))
        period 3
        scaling-factor (get-period-scaling-factor period total-dur 60)]
    (->> durs (map #(* % scaling-factor))
         (apply +)
         (= period))))

(comment
  (def canon (converge {:durs (flatten (repeat 300 [1/4 1]))
                        :tempos [5 4 2 1]
                        :cps [150]}))
  (def canon-2 (converge {:durs (repeat 5 1)
                          :tempos [1 2]
                          :cps [3]}))
  (def canon-3 (converge {:durs (repeat 5 1)
                          :tempos [1 2]
                          :cps [3]
                          :period 3
                          :bpm 60}))
  (def canon-4 (converge {:durs (repeat 5 1)
                          :tempos [1 2]
                          :cps [3]
                          :period 3
                          :bpm 120}))
  (meta canon-2)
  (meta canon-3)
  (meta canon-4)
  (canon-3)
  (canon-dur canon-2 60))


(comment ;; test that all voices have a remainder and their duration is the same
  (->> @user/data :canon user/spy
       (map (comp #(apply + %)
                  (fn [voice] (filter #(not= true %) voice))
                  vals
                  last))
       (apply =)))



(comment
  ;; nucleo del converge original traducido a clj
  (defn make-bcp [cp line] (sum (subvec line 0 (max 0 (- cp 1)))))

  (defn make-tempo [speed] (/ 60 (/ speed 4)))

  (defn total-dur [voice] (sum (map :dur (:melody voice))))

  (defn make-voices [{:keys [cp voices melody]}]
    (->> voices
         (map (fn [voice] ; define each melody event for each voice
                (->> melody
                     (map (fn [event]
                            {:dur (* (:dur event) (make-tempo (:tempo voice)))
                             :note (+ (:note event) (:transp voice))
                             :amp (:amp event)
                             :cp cp})))))
         (map (fn [voice] ; add bcp (before convergence point)
                (let [bcp  (make-bcp cp (mapv :dur voice))]
                  {:melody voice :bcp bcp})))
         (sort (fn [a, b] (> (total-dur a) (total-dur b)))); sort voices by duration
         ((fn [voices] ; add onset data
            (let [min-bcp  (apply min (map #(get-in % [:bcp]) voices))]
              (map #(assoc % :onset (Math/abs (float (- min-bcp (:bcp %))))) voices))))))

  (user/spy (make-voices
             {:cp 2
              :voices [{:tempo 60 :transp 0} {:tempo 50 :transp 7}]
              :melody [{:dur 1 :note 60} {:dur 2 :note 62}]}))

  (defn converge [symbol melody cp voices instruments player repeat osc meta] melody)

  (make-bcp 3 [0 1 2 3 4]))
