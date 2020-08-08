(ns time-time.dynacan.players
  (:require
   [time-time.dynacan.core :refer [get-event-at find-first-event-using-cp]]
   [time-time.sequencing-3 :as s]
   [overtone.music.time :refer [now]]
   [time-time.player :as p :refer [player]]
   [taoensso.timbre :as log]
   [time-time.standard :refer [wrap-at]]
   [clojure.set :as set]))


(defn reciprocal [n] (/ 1 n))

(deftype std [canon-atoms]
  p/player
  (loop? [this] (doseq [voice-atom canon-atoms]
                  (:loop? @voice-atom)))
  (stop! [this] (doseq [voice-atom canon-atoms]
                  (swap! voice-atom assoc :playing? false)))
  (data [this] canon-atoms))

(defn std! [durs ratios cp on-event
            & {:keys [start-time loop? tempo]
               :or {start-time 0
                    loop? false
                    tempo 60}}]
  (let [[ref-ratio & sub-ratios] (->> ratios
                                      (map reciprocal)
                                      (sort-by identity >))
        cp-elapsed-at (:elapsed (get-event-at ref-ratio durs cp))
        start-time* (+ start-time (now))
        total-dur (apply + durs)
        canon-dur (* ref-ratio total-dur)
        canon (into []
                    (map-indexed
                     (fn [i r]
                       (let [{:keys [index elapsed] :as data} (find-first-event-using-cp
                                                               r durs cp cp-elapsed-at
                                                               :loop? false)
                             total-dur* (* total-dur r)
                             ;; silent duration after the voice has ended
                             ;; and before the whole canon ends
                             rest* (- canon-dur total-dur* elapsed)]
                         (s/play! durs on-event
                                  :ratio r
                                  :start-time start-time*
                                  :start-index index
                                  :elapsed elapsed
                                  :loop? loop?
                                  :tempo tempo
                                  :extra-data
                                  (-> data
                                      (set/rename-keys
                                       {:index :initial-index
                                        :elapsed :start-delay})
                                      (assoc :voice i
                                             :canon-dur canon-dur
                                             :total-dur total-dur*
                                             :rest rest*))
                                  :before-update
                                  (fn [{:as data {dur :dur} :current-event}]
                                    (-> data
                                        (update :echoic-distance-event-qty dec)
                                        (update :echoic-distance - dur)))
                                  :on-schedule (fn [data event-schedule]
                                                 (if (and (last-event? data)
                                                          (:loop? data))
                                                   (do (println "updating")
                                                       ( + event-schedule rest*
                                                        (:start-delay data)))
                                                   event-schedule)))))
                     (conj sub-ratios ref-ratio)))]
    (std. canon)))
(do
  (defn cp? [{:keys [cp index durs]}]
    (= cp (mod index (count durs))))
  (is-cp? {:durs [1 2 3 4] :cp 2 :index 6})
  (defn last-event? [{:keys [index durs]}]
    (= (dec (count durs)) (mod index (count durs))))
  (last-event? {:durs [1 2 3 4] :cp 2 :index 7})
  )


;; TODO calculate remaining time of non `ref-ratio` voice and figure out how to loop voices using that
(comment

  (def canon
    (std! [1 1 1 3 4] [1 2 3] 3
          (fn [{data :data}]
            (when (is-cp? data)
              (log/info "Is cp:" (:voice data))))
          :loop? true)))

(comment
  (-> canon p/stop!)
  (-> canon p/data first deref)
  (-> canon p/loop?))
