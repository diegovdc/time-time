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
        make-v2 (fn [v1 events-from-cp* event-chan & {:keys [cp loop?]
                                                      :or {cp nil loop? false}}]
                  (add-to! @v1 events-from-cp*
                           (fn [{:keys [data]}]
                             (a/>!! event-chan (assoc data ::voice :v-2 ::now (now))))
                           {:durs v2-durs :ratio 1/3 :cp cp  :loop? loop?}))
        compile-index 4 ;; NOTE compile time (of ref-voice) for  v2. v2 will start at ref-voice index 5 at the earliest.
        ]

    (testing "Non-looping `v2`"
      (testing "`events-from-cp*` is 0 so cp should happen on ref-voice's index 5"
        (let [compiler-chan (a/chan)
              first-event-after-compilation (inc compile-index)
              events-from-cp* 0
              final-index  (+ 1 first-event-after-compilation events-from-cp*)
              {:keys [event-chan result-chan stop-chan]} (async-events-tester-2)
              v1 (make-ref-voice compiler-chan compile-index event-chan
                                 (stop-at-index stop-chan final-index))
              _ (a/<!! compiler-chan)
              v2 (make-v2 v1 events-from-cp* event-chan)
              _ (log/info "corre")
              results (group-by ::voice (a/<!! result-chan))]
          (stop! v1 v2)
          (is (close-to (-> results :ref (nth (inc compile-index)) ::now)
                        5
                        (-> results :v-2 (nth 0) ::now)))))

      (testing "`events-from-cp*` is greater than zero. `v2-cp`: selected at random within the voice durs indexes. `events-from-cp*` is 5 so cp should happen on ref-voice's index 10 (because `compile-index` is 4)"
        (let [compiler-chan (a/chan)
              first-event-after-compilation (inc compile-index)
              events-from-cp* 5
              final-index  (+ 1 first-event-after-compilation events-from-cp*)
              {:keys [event-chan result-chan stop-chan]} (async-events-tester-2)
              v1 (make-ref-voice compiler-chan compile-index event-chan
                                 (stop-at-index stop-chan final-index))
              _ (a/<!! compiler-chan)
              v2-cp (rand-nth [0 1])
              v2 (make-v2 v1 events-from-cp* event-chan :cp v2-cp)
              results* (a/<!! result-chan)
              results (group-by ::voice results*)]
          (stop! v1 v2)
          (is (close-to (-> results :ref (nth (+ events-from-cp*
                                                 first-event-after-compilation))
                            ::now)
                        5
                        (-> results :v-2 (nth v2-cp) ::now)))))

      (testing "`v2-cp`: beyond voice durs indexes, `add-to!` will use modulo arithmetic to determine the real cp. "
        (let [compiler-chan (a/chan)
              first-event-after-compilation (inc compile-index)
              events-from-cp* 5
              final-index  (+ 1 first-event-after-compilation events-from-cp*)
              {:keys [event-chan result-chan stop-chan]} (async-events-tester-2)
              v1 (make-ref-voice compiler-chan compile-index event-chan
                                 (stop-at-index stop-chan final-index))
              _ (a/<!! compiler-chan)
              v2-cp (rrand 3 10)
              v2 (make-v2 v1 events-from-cp* event-chan :cp v2-cp)
              results* (a/<!! result-chan)
              results (group-by ::voice results*)]
          (stop! v1 v2)
          (is (close-to (-> results :ref (nth (+ events-from-cp*
                                                 first-event-after-compilation))
                            ::now)
                        5
                        (-> results :v-2 (nth (mod v2-cp (count v2-durs))) ::now))))))

    (testing "Looping `v2`"
      (let [compiler-chan (a/chan)
            first-event-after-compilation (inc compile-index)
            events-from-cp* 2
            final-index  (+ 1 first-event-after-compilation events-from-cp*)
            {:keys [event-chan result-chan stop-chan]} (async-events-tester-2)
            v1 (make-ref-voice compiler-chan compile-index event-chan
                               (stop-at-index stop-chan final-index))
            _ (a/<!! compiler-chan)
            v2-cp (rrand 7 10)
            v2-cp-real-index 10 ;; NOTE hard-coded for practicity
            v2 (make-v2 v1 events-from-cp* event-chan :cp v2-cp :loop? true)
            results* (a/<!! result-chan)
            results (group-by ::voice results*)]
        (stop! v1 v2)

        (testing "If `v2` loops there is a `cp`"
          (is (close-to (-> results :ref
                            (nth (+ events-from-cp* first-event-after-compilation)) ;; NOTE `index` 6
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

    (testing "Adding a third voice"
      (testing "`v1` is the ref-voice for `v2` and `v3`, each voice has a different `cp` with respect to `v1`."
        (let [compiler-chan (a/chan)
              cps (atom {})
              ->cps (fn [voice event]
                      (when (= (:echoic-distance-event-qty event) 0)
                        (swap! cps assoc voice event)))
              events-from-cp* 5
              compile-index-1 4
              compile-index-2 9
              first-event-after-compilation-v2 (inc compile-index-1)
              first-event-after-compilation-v3 (inc compile-index-2)
              {:keys [event-chan result-chan stop-chan]} (async-events-tester-2)
              v1 (s/play! [2 3]
                          (fn [{:keys [data]}]
                            (start-voice-fn! compiler-chan data compile-index-1)
                            (start-voice-fn! compiler-chan data compile-index-2)
                            (a/>!! event-chan (assoc data ::voice :ref ::now (now))))
                          :loop? true
                          :ratio 1
                          :tempo 2400)
              _ (a/<!! compiler-chan)
              v2-cp (rrand 3 10)
              v2 (add-to! @v1 events-from-cp*
                          (fn [{:keys [data]}]
                            (->cps :v2 data)
                            (a/>!! event-chan (assoc data ::voice :v-2 ::now (now))))
                          {:durs v2-durs :ratio 11/13 :cp v2-cp :loop? true})
              _ (a/<!! compiler-chan)
              v3-cp (rrand 3 10)
              v3 (add-to! @v1 events-from-cp*
                          (fn [{:keys [data]}]
                            (a/>!! event-chan (assoc data ::voice :v-3 ::now (now)))
                            (->cps :v3 data)
                            (when (= (:events-from-cp data) -1)
                              (a/>!! stop-chan true))
                            ((stop-at-index stop-chan (-> @v1 :index inc (+ events-from-cp*))) data))
                          {:durs [3 5] :ratio 5/7 :cp v3-cp :loop? true})
              results* (a/<!! result-chan)
              results (group-by ::voice results*)]
          (stop! v1 v2  v3)
          (is (close-to (-> results :ref (nth (+ events-from-cp*
                                                 first-event-after-compilation-v2))
                            ::now)
                        5
                        (-> results :v-2 (nth 10) ::now)))
          (is (close-to (-> results :ref (nth (+ events-from-cp*
                                                 first-event-after-compilation-v3))
                            ::now)
                        5
                        (-> results :v-3 (nth 4) ::now)))))
      (testing "`v1` is the ref-voice for `v2` and `v2` is ref-voice for `v3`"
        (let [compiler-chan (a/chan)
              cps (atom {})
              ->cps (fn [voice event]
                      (when (= (:echoic-distance-event-qty event) 0)
                        (swap! cps assoc voice event)))
              events-from-cp* 5
              compile-index-1 4
              compile-index-2 9
              first-event-after-compilation-v2 (inc compile-index-1)
              {:keys [event-chan result-chan stop-chan]} (async-events-tester-2)
              v1 (s/play! [2 3]
                          (fn [{:keys [data]}]
                            (start-voice-fn! compiler-chan data compile-index-1)
                            (start-voice-fn! compiler-chan data compile-index-2)
                            (a/>!! event-chan (assoc data ::voice :ref ::now (now))))
                          :loop? true
                          :ratio 1
                          :tempo 2400)
              _ (a/<!! compiler-chan)
              v2-cp (rrand 3 10)
              v2 (add-to! @v1 events-from-cp*
                          (fn [{:keys [data]}]
                            (->cps :v2 data)
                            (a/>!! event-chan (assoc data ::voice :v-2 ::now (now))))
                          {:durs v2-durs :ratio 11/13 :cp v2-cp :loop? true})
              _ (a/<!! compiler-chan)
              v3-cp (rrand 3 10)
              v3 (add-to! @v2 events-from-cp*
                          (fn [{:keys [data]}]
                            (a/>!! event-chan (assoc data ::voice :v-3 ::now (now)))
                            (->cps :v3 data)
                            (when (= (:events-from-cp data) -1)
                              (a/>!! stop-chan true))
                            ((stop-at-index stop-chan (-> @v1 :index inc (+ events-from-cp*))) data))
                          {:durs [3 5] :ratio 5/7 :cp v3-cp :loop? true})
              results* (a/<!! result-chan)
              results (group-by ::voice results*)]
          (stop! v1 v2 v3)
          (is (close-to (-> results :ref (nth (+ events-from-cp*
                                                 first-event-after-compilation-v2))
                            ::now)
                        5
                        (-> results :v-2 (nth 10) ::now)))
          (is (close-to (-> results :v-2 (nth 13) ::now)
                        5
                        (-> results :v-3 (nth 2) ::now)))))
      (testing "(Non-looping version) `v1` is the ref-voice for `v2` and `v3`. Both are compiled at the same time and should therefore have the same cp. NOTE `v1` must be dereferenced once."
        (let [compiler-chan (a/chan)
              first-event-after-compilation (inc compile-index)
              events-from-cp* 5
              final-index  (+ 1 first-event-after-compilation events-from-cp*)
              {:keys [event-chan result-chan stop-chan]} (async-events-tester-2)
              v1 (make-ref-voice compiler-chan compile-index event-chan
                                 (stop-at-index stop-chan final-index))
              _ (a/<!! compiler-chan)
              cp 1
              v1-data @v1
              v2 (add-to! v1-data events-from-cp*
                          (fn [{:keys [data]}]
                            (a/>!! event-chan (assoc data ::voice :v-2 ::now (now))))
                          {:durs v2-durs :ratio 11/13 :cp cp :loop? false})
              v3 (add-to! v1-data events-from-cp*
                          (fn [{:keys [data]}]
                            (a/>!! event-chan (assoc data ::voice :v-3 ::now (now))))
                          {:durs [1 5] :ratio 11/15 :cp cp :loop? false})
              results* (a/<!! result-chan)
              results (group-by ::voice results*)]
          (stop! v1 v2 v3)
          (is (close-to (-> results :ref (nth (+ events-from-cp*
                                                 first-event-after-compilation))
                            ::now)
                        5
                        (-> results :v-2 (nth cp) ::now)))
          (is (close-to (-> results :ref (nth (+ events-from-cp*
                                                 first-event-after-compilation))
                            ::now)
                        5
                        (-> results :v-3 (nth cp) ::now)))))
      (testing "(Looping version) `v1` is the ref-voice for `v2` and `v3`. Both are compiled at the same time and should therefore have the same cp. NOTE `v1` must be dereferenced once."
        (let [compiler-chan (a/chan)
              first-event-after-compilation (inc compile-index)
              events-from-cp* 5
              final-index  (+ 1 first-event-after-compilation events-from-cp*)
              {:keys [event-chan result-chan stop-chan]} (async-events-tester-2)
              v1 (make-ref-voice compiler-chan compile-index event-chan
                                 (stop-at-index stop-chan final-index))
              _ (a/<!! compiler-chan)
              cp 1
              v1-data @v1
              v2 (add-to! v1-data events-from-cp*
                          (fn [{:keys [data]}]
                            (a/>!! event-chan (assoc data ::voice :v-2 ::now (now))))
                          {:durs v2-durs :ratio 11/13 :cp cp :loop? true})
              v3 (add-to! v1-data events-from-cp*
                          (fn [{:keys [data]}]
                            (a/>!! event-chan (assoc data ::voice :v-3 ::now (now))))
                          {:durs [1 5] :ratio 11/15 :cp cp :loop? true})
              results* (a/<!! result-chan)
              results (group-by ::voice results*)]
          (stop! v1 v2 v3)
          (is (close-to (-> results :ref (nth (+ events-from-cp*
                                                 first-event-after-compilation))
                            ::now)
                        5
                        (-> results :v-2 (nth 10) ::now)))
          (is (close-to (-> results :ref (nth (+ events-from-cp*
                                                 first-event-after-compilation))
                            ::now)
                        5
                        (-> results :v-3 (nth 5) ::now))))))))

(comment (->> @user/data :results (map (juxt ::voice :index ::now)))
         (-> @user/data :results (->> (group-by ::voice)) :v-2 (nth 10))
         (-> @user/data :cps (->> (map (juxt) :index)))
         (->  (group-by ::voice (@user/data :results)) :ref (nth 5) ::now)
         (->  (group-by ::voice (@user/data :results)) :v-2 (nth 0) ::now))
