(ns time-time.dynacan.players
  (:require
   [time-time.dynacan.core :refer [get-event-at find-first-event-using-cp]]
   [time-time.sequencing-3 :as s]
   [overtone.music.time :refer [now]]
   [time-time.player :as p :refer [player]]
   [time-time.standard :refer [wrap-at dur->ms]]
   [clojure.set :as set]))


(deftype std [canon-atoms]
  p/player
  (loop? [this] (doseq [voice-atom canon-atoms]
                  (:loop? @voice-atom)))
  (stop! [this] (doseq [voice-atom canon-atoms]
                  (swap! voice-atom assoc :playing? false)))
  (data [this] canon-atoms))

(declare +extra-data before-update reciprocal)

(defn std! [durs ratios cp on-event
            & {:keys [start-time loop? tempo]
               :or {start-time 0
                    loop? false
                    tempo 60}}]
  (let [ratios (->> ratios (map reciprocal) (sort-by identity >))
        ref-ratio (first ratios)
        cp-elapsed-at (:elapsed (get-event-at ref-ratio durs cp))
        start-time* (+ start-time (now))
        total-dur (apply + durs)
        canon-dur (* ref-ratio total-dur)
        canon (into []
                    (map-indexed
                     (fn [voice-index r]
                       (let [{:keys [index elapsed] :as data} (find-first-event-using-cp
                                                               r durs cp cp-elapsed-at
                                                               :loop? false)

                             total-dur* (* total-dur r)
                             ;; silent duration after the voice has ended
                             ;; and before the whole canon ends
                             ;; it is an absolute duration with respect
                             ;; to the `total-dur`
                             rest* (- canon-dur total-dur* elapsed)]
                         (s/play! durs on-event
                                  :ratio r
                                  :start-time start-time*
                                  :start-index index
                                  :elapsed elapsed
                                  :loop? loop?
                                  :tempo tempo
                                  :extra-data (+extra-data voice-index
                                                           canon-dur
                                                           total-dur*
                                                           rest*
                                                           data)
                                  :before-update (partial before-update rest*
                                                          ref-ratio))))
                     ratios))]
    (std. canon)))

(defn reciprocal [n] (/ 1 n))

(defn loop-restart? [{:keys [index durs]}]
  (and (not= 0 index) (= 0 (mod index (count durs)))))

(defn add-delay-on-loop-repeat
  "Addss the rest and start-delay values for voices on repeat.
  Necessary for all the voices that are not the reference voice"
  [{:keys [elapsed start-delay tempo] :as data}
   rest* ref-ratio]
  (if (and (loop-restart? data) (:loop? data))
    (assoc data :elapsed
           (+ elapsed
              (dur->ms (+ rest*
                          start-delay)
                       tempo)))
    data))

(defn +extra-data
  [voice-index canon-dur total-dur* rest* data]
  (-> data
      (set/rename-keys
       {:index :initial-index
        :elapsed :start-delay})
      (assoc :voice voice-index
             :canon-dur canon-dur
             :total-dur total-dur*
             :rest rest*)))

(defn before-update
  [rest* ref-ratio {:as data {dur :dur} :current-event}]
  (-> data
      (add-delay-on-loop-repeat rest* ref-ratio)
      (update :echoic-distance-event-qty dec)
      (update :echoic-distance - dur)))


(comment
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
