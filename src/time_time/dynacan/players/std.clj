(ns time-time.dynacan.players.std
  (:require
   [time-time.dynacan.core :refer [get-event-at find-first-event-using-cp]]
   [clojure.spec.alpha :as spec]
   [time-time.sequencing-3 :as s]
   [overtone.music.time :refer [now]]
   [time-time.player :as p :refer [player]]
   [time-time.standard :refer [wrap-at dur->ms]]
   [clojure.set :as set]
   [taoensso.timbre :as log]))


(deftype std [canon-atoms]
  p/player
  (loop? [this] (doseq [voice-atom canon-atoms]
                  (:loop? @voice-atom)))
  (stop! [this] (doseq [voice-atom canon-atoms]
                  (swap! voice-atom assoc :playing? false)))
  (data [this] canon-atoms))

(declare +extra-data before-update reciprocal std-2!- std!-)

(spec/def ::dur (spec/and number? #(> % 0)))
(spec/def ::durs (spec/and (spec/coll-of ::dur) (comp not empty?)))
(spec/def ::durs-vec (spec/coll-of ::durs))

(spec/valid? ::durs-vec [])

(defn std!
  ([durs ratios cp on-event] (std! durs ratios cp on-event {}))
  ([durs ratios cp on-event & {:as opts}]
   (let [opts* (merge {:start-time 0 :loop? false :tempo 60} opts)]
     (cond
       (spec/valid? ::durs-vec durs) (std-2!-  durs ratios cp on-event opts*)
       (spec/valid? ::durs durs) (std!- durs ratios cp on-event opts*)
       :default (throw (ex-info (spec/explain-str ::durs durs) {:durs durs}))))))

(declare +extra-data)
(defn std!- [durs ratios cp on-event {:keys [start-time loop? tempo]}]
  (let [ratios (->> ratios (map reciprocal) (sort-by identity >))
        ref-ratio (first ratios)
        cp-elapsed (:elapsed (get-event-at ref-ratio durs cp))
        start-time* (+ start-time (now))
        total-dur (apply + durs)
        canon
        (into []
              (map-indexed
               (fn [voice-index ratio]
                 (let [{:keys [index elapsed]
                        :as first-event-data} (find-first-event-using-cp
                                               ratio
                                               durs
                                               cp
                                               cp-elapsed
                                               :loop? false)]
                   (s/play! durs on-event
                            :ratio ratio
                            :start-time start-time*
                            :start-index index
                            :elapsed elapsed
                            :loop? loop?
                            :tempo tempo
                            :before-update  before-update
                            :extra-data (+extra-data total-dur
                                                     ref-ratio
                                                     first-event-data
                                                     voice-index
                                                     ratio
                                                     tempo))))
               ratios))]
    (std. canon)))

(defn +extra-data [total-dur ref-ratio first-event-data voice-index ratio tempo]
  (let [elapsed (first-event-data :elapsed)
        total-dur* (* total-dur ratio)
        canon-dur (* ref-ratio total-dur)
        rest* (- canon-dur total-dur* elapsed)
        rest-ms (dur->ms rest* tempo)
        start-delay-ms (dur->ms elapsed tempo)]
    (-> first-event-data
        (set/rename-keys
         {:index :initial-index
          :elapsed :start-delay})
        (assoc :voice voice-index
               :canon-dur canon-dur
               :total-dur total-dur*
               ;; `:rest` is the silent duration after the voice has ended and before the whole canon ends it is an absolute duration with respect to the `total-dur`
               :rest rest*
               :rest-ms rest-ms
               :start-delay-ms start-delay-ms))))

(defn reciprocal [n] (/ 1 n))

(defn loop-restart? [{:keys [index durs]}]
  (and (not= 0 index) (= 0 (mod index (count durs)))))

(defn add-delay-on-loop-repeat
  "Adds the `rest-ms` and `start-delay-ms` values for voices on repeat.
  Necessary for all the voices that are not the reference voice"
  [{:keys [elapsed-ms start-delay-ms rest-ms] :as data}]
  (if (and (loop-restart? data) (:loop? data))
    (assoc data :elapsed-ms (+ elapsed-ms start-delay-ms rest-ms))
    data))

(defn before-update
  [{:as data {dur :dur} :current-event}]
  (-> data
      add-delay-on-loop-repeat
      (update :events-from-cp dec)
      (update :interval-from-cp - dur)))


(comment
  (require '[taoensso.timbre :as log])
  (defn cp? [{:keys [cp index durs]}]
    (= cp (mod index (count durs))))
  (def canon
    (std! [1 1 4 1] [2 3 5] 2
          (fn [{data :data}]
            (when (cp? data) (log/info "cp!" (:voice data))))
          :loop? true)))

(comment
  (-> canon p/stop!)
  (-> canon p/data first deref)
  (-> canon p/loop?))
