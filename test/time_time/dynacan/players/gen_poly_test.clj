(ns time-time.dynacan.players.gen-poly-test
  (:require  [clojure.test :refer [deftest testing is]]
             [time-time.dynacan.players.gen-poly :refer [add-to!]]
             [time-time.utils.async :refer [async-events-tester-2]]
             [time-time.utils.core :refer [close-to get-time-interval]]
             [time-time.sequencing-3 :as s]
             [taoensso.timbre :as log]
             [time-time.standard :refer [now rrand]]
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
                         (s/play! [2 3]
                                  (fn [{:keys [data]}]
                                    (start-voice-fn! compiler-chan data compile-index)
                                    (a/>!! event-chan (assoc data ::voice :ref ::now (now)))
                                    (stop-fn data))
                                  :loop? true
                                  :ratio 1
                                  :tempo 2400))
        v2-durs [1 2]
        make-v2 (fn [v1 echoic-distance-qty event-chan & {:keys [cp loop?]
                                                         :or {cp nil loop? false}}]
                  (add-to! @v1 echoic-distance-qty
                           (fn [{:keys [data]}]
                             (a/>!! event-chan (assoc data ::voice :v-2 ::now (now))))
                           {:durs v2-durs :ratio 1/3 :cp cp  :loop? loop?}))
        compile-index 4 ;; NOTE compile time (of ref-voice) for  v2. v2 will start at ref-voice index 5 at the earliest.
        ]

    (testing "Non-looping `v2`"

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
          (is (close-to (-> results :ref (nth (inc compile-index)) ::now)
                        5
                        (-> results :v-2 (nth 0) ::now)))))

      (testing "`echoic-distance-qty` is greater than zero. `v2-cp`: selected at random within the voice durs indexes. `echoic-distance-qty` is 5 so cp should happen on ref-voice's index 10 (because `compile-index` is 4)"
        (let [compiler-chan (a/chan)
              first-event-after-compilation (inc compile-index)
              echoic-distance-qty 5
              final-index  (+ 1 first-event-after-compilation echoic-distance-qty)
              {:keys [event-chan result-chan stop-chan]} (async-events-tester-2)
              v1 (make-ref-voice compiler-chan compile-index event-chan
                                 (stop-at-index stop-chan final-index))
              _ (a/<!! compiler-chan)
              v2-cp (rand-nth [0 1])
              v2 (make-v2 v1 echoic-distance-qty event-chan :cp v2-cp)
              results* (a/<!! result-chan)
              results (group-by ::voice results*)]
          (stop! v1 v2)
          (is (close-to (-> results :ref (nth (+ echoic-distance-qty
                                                 first-event-after-compilation))
                            ::now)
                        5
                        (-> results :v-2 (nth v2-cp) ::now)))))

      (testing "`v2-cp`: beyond voice durs indexes, `add-to!` will use modulo arithmetic to determine the real cp. "
        (let [compiler-chan (a/chan)
              first-event-after-compilation (inc compile-index)
              echoic-distance-qty 5
              final-index  (+ 1 first-event-after-compilation echoic-distance-qty)
              {:keys [event-chan result-chan stop-chan]} (async-events-tester-2)
              v1 (make-ref-voice compiler-chan compile-index event-chan
                                 (stop-at-index stop-chan final-index))
              _ (a/<!! compiler-chan)
              v2-cp (rrand 3 10)
              v2 (make-v2 v1 echoic-distance-qty event-chan :cp v2-cp)
              results* (a/<!! result-chan)
              results (group-by ::voice results*)]
          (stop! v1 v2)
          (is (close-to (-> results :ref (nth (+ echoic-distance-qty
                                                 first-event-after-compilation))
                            ::now)
                        5
                        (-> results :v-2 (nth (mod v2-cp (count v2-durs))) ::now))))))

    (testing "Looping `v2`"
      (let [compiler-chan (a/chan)
            first-event-after-compilation (inc compile-index)
            echoic-distance-qty 2
            final-index  (+ 1 first-event-after-compilation echoic-distance-qty)
            {:keys [event-chan result-chan stop-chan]} (async-events-tester-2)
            v1 (make-ref-voice compiler-chan compile-index event-chan
                               (stop-at-index stop-chan final-index))
            _ (a/<!! compiler-chan)
            v2-cp (rrand 7 10)
            v2-cp-real-index 10 ;; NOTE hard-coded for practicity
            v2 (make-v2 v1 echoic-distance-qty event-chan :cp v2-cp :loop? true)
            results* (a/<!! result-chan)
            results (group-by ::voice results*)]
        (stop! v1 v2)

        (testing "If `v2` loops there is a `cp`"
          (is (close-to (-> results :ref
                            (nth (+ echoic-distance-qty first-event-after-compilation)) ;; NOTE `index` 6
                            ::now)
                        5
                        (-> results :v-2 (nth v2-cp-real-index) ::now))))

        (testing "`cp` is chosen in modulo form. `v-2` durs are [1/3 2/3] so depending if v-2 `cp` is 9 or 10 (odd/even) the the dur a the cp event changes."
          ;; TODO make this test more clear
          (is (= (mod v2-cp (count v2-durs))
                 (mod (-> results  :v-2 (nth v2-cp-real-index) :index) ;; get the real index
                      (count v2-durs)))))

        (testing "`v2` will start as early as possible (in this case, at the same time as the `first-event-after-compilation`"
          (is (close-to (-> results :ref (nth first-event-after-compilation) ::now)
                        5
                        (-> results :v-2 first ::now))))))
    ;; TODO add v3
    (testing "Adding a third voice")))

(comment (->> @user/data :results (map (juxt ::voice :index ::now )) )
         (-> @user/data :results (->> (group-by ::voice)) :v-2 #_(nth 10))
         (->  (group-by ::voice (@user/data :results)) :ref (nth 5) ::now)
         (->  (group-by ::voice (@user/data :results)) :v-2 (nth 0) ::now))
(mod 9 2)

34/17
