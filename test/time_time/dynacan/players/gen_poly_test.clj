(ns time-time.dynacan.players.gen-poly-test
  (:require  [clojure.test :refer [deftest testing is]]
             [time-time.dynacan.players.gen-poly :refer [add-to!]]
             [time-time.utils.async :refer [async-events-tester-2]]
             [time-time.utils.core :refer [close-to get-time-interval]]
             [time-time.sequencing-3 :as s]
             [taoensso.timbre :as log]
             [time-time.standard :refer [now]]
             [clojure.core.async :as a]))

(defn start-voice-fn! [chan voice-data index]
  (if (= (voice-data :index) index)
    (a/go (a/>! chan true))))

(defn stop! [& voices] (doseq [v voices] (swap! v assoc :playing? false)))

(defn stop-at-index
  "Returns a function that takes events and tests if it should stop"
  [stop-chan index]
  (fn [event] (when (= (:index event) index)
               (a/>!! stop-chan true))))

(deftest add-to!-test
  (let [make-ref-voice (fn [compiler-chan compile-index event-chan stop-fn]
                         (s/play! [1 1 1]
                                  (fn [{:keys [data]}]
                                    (start-voice-fn! compiler-chan data compile-index)
                                    (a/>!! event-chan (assoc data ::voice :ref ::now (now)))
                                    (stop-fn data))
                                  :loop? true
                                  :ratio 1
                                  :tempo 600))
        make-v2 (fn [v1 echoic-distance-qty event-chan & {:keys [cp loop?] :or {cp nil loop? false}}]
                  (add-to! @v1 echoic-distance-qty
                           (fn [{:keys [data]}]
                             (a/>!! event-chan (assoc data ::voice :v-2 ::now (now))))
                           {:durs [1 2] :ratio 1/3 :cp cp  :loop? loop?}))
        compile-index 4]
    (testing "`echoic-distance-qty` is 0 so cp should happen on ref-voice's index 5"
      (let [compiler-chan (a/chan)
            first-event-after-compilation (inc compile-index)
            echoic-distance-qty 0
            final-index  (+ 1 first-event-after-compilation echoic-distance-qty)
            {:keys [event-chan result-chan stop-chan]} (async-events-tester-2)
            v1 (make-ref-voice compiler-chan compile-index event-chan
                               (stop-at-index stop-chan final-index))
            _ (a/<!! compiler-chan)
            v2 (make-v2 v1 echoic-distance-qty event-chan)
            results (group-by ::voice (a/<!! result-chan))]
        (stop! v1 v2)
        (is (close-to (-> results :ref (nth 5) ::now)
                      5
                      (-> results :v-2 (nth 0) ::now)))))
    ;; TODO cp tests: par, non, out of range...
    ))

(comment (->> @user/data :results (map (juxt ::voice :index ::now )))
         (-> @user/data :results :ref )
         (->  (group-by ::voice (@user/data :results)) :ref (nth 5) ::now)
         (->  (group-by ::voice (@user/data :results)) :v-2 (nth 0) ::now))
