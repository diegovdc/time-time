(ns time-time.sequencing-3-test
  (:require
   [clojure.core.async :as a]
   [clojure.test :refer [deftest is testing]]
   [time-time.sequencing-3
    :refer
    [calculate-next-voice-state play-event? schedule!]]
   [time-time.standard :refer [now wrap-at]]
   [time-time.utils.async :refer [async-events-tester]]
   [time-time.utils.core :refer [close-to get-time-interval]]))

(deftest calculate-next-voice-state-test
  (testing "Increases the `index` by one, updates the `elapsed` value and updates (or sets) the `current-event-dur`"
    (is (= {:durs [1 2 1],
            :index 1,
            :tempo 60,
            :loop? false,
            :started-at 0,
            :elapsed-ms 1000,
            :ratio 1,
            :current-event {:dur-ms 1000, :dur 1}}
           (calculate-next-voice-state {:durs [1 2 1]
                                        :index 0
                                        :tempo 60
                                        :loop? false
                                        :started-at 0
                                        :elapsed-ms 0
                                        :ratio 1})))
    (is (= {:durs [1 2 1],
            :index 2,
            :tempo 60,
            :loop? false,
            :started-at 0,
            :elapsed-ms 3000,
            :ratio 1,
            :current-event {:dur-ms 2000, :dur 2}}
           (calculate-next-voice-state {:durs [1 2 1],
                                        :index 1,
                                        :tempo 60,
                                        :loop? false,
                                        :started-at 0,
                                        :elapsed-ms 1000,
                                        :ratio 1}))))
  (testing "`current-event-dur` is related to tempo"
    (is (= {:dur-ms 500N, :dur 1}
           (:current-event
            (calculate-next-voice-state {:durs [1 2 1]
                                         :index 0
                                         :tempo 120
                                         :loop? false
                                         :started-at 0
                                         :elapsed-ms 0
                                         :ratio 1}))))
    (is (= {:dur-ms 1000N, :dur 2}
           (:current-event
            (calculate-next-voice-state {:durs [1 2 1]
                                         :index 1
                                         :tempo 120
                                         :loop? false
                                         :started-at 0
                                         :elapsed-ms 0
                                         :ratio 1}))))
    (is (= {:dur-ms 4000, :dur 2}
           (:current-event
            (calculate-next-voice-state {:durs [1 2 1]
                                         :index 1
                                         :tempo 30
                                         :loop? false
                                         :started-at 0
                                         :elapsed-ms 0
                                         :ratio 1})))))
  (testing "Different ratio"
    (is (= {:dur-ms 250N, :dur 1/2}
           (:current-event
            (calculate-next-voice-state {:durs [1 2 1]
                                         :index 0
                                         :tempo 120
                                         :loop? false
                                         :started-at 0
                                         :elapsed-ms 0
                                         :ratio 1/2}))))
    (is (= {:dur-ms 500N, :dur 1}
           (:current-event
            (calculate-next-voice-state {:durs [1 2 1]
                                         :index 1
                                         :tempo 120
                                         :loop? false
                                         :started-at 0
                                         :elapsed-ms 0
                                         :ratio 1/2})))))
  (testing "`:durs` can be a function"
    (is (= {:dur-ms 250N, :dur 1/2}
           (:current-event
            (calculate-next-voice-state {:durs (fn [{:keys [index ratio] :as _voice}]
                                                 (* ratio (wrap-at index [1 2 1])))
                                         :index 0
                                         :tempo 120
                                         :loop? false
                                         :started-at 0
                                         :elapsed-ms 0
                                         :ratio 1/2}))))))

(deftest play-event?-test
  (testing "Play a first and second event but not a third nor fourth"
    (is (true? (play-event? 0 [1 2] false)))
    (is (true? (play-event? 1 [1 2] false)))
    (is (false? (play-event? 2 [1 2] false)))
    (is (false? (play-event? 3  [1 2] false))))
  (testing "Loop playing (always play events)"
    (is (true? (play-event? 0 [1 2] true)))
    (is (true? (play-event? 1 [1 2] true)))
    (is (true? (play-event? 2 [1 2] true)))
    (is (true? (play-event? 3  [1 2] true)))))

(defn get-dur [event]
  ((event :durs) (event :index)))

(deftest schedule!-test
  (let [base-voice {:durs [1 2 1 2 1 1]
                    :index 0
                    :tempo 6000 ;; NOTE a dur of `1` lasts `10N`ms
                    :loop? false
                    :started-at 0
                    :elapsed-ms 0
                    :ratio 1
                    :playing? true}
        default-continue (fn [ev _] (< (ev :index)
                                       (dec (count (base-voice :durs)))))]
    (testing "`:elapsed-ms` values are correct"
      (let [{:keys [event-chan result-chan]} (async-events-tester
                                              default-continue)
            v (atom (assoc base-voice
                           :started-at (+ 1 (now))
                           :on-event (fn [{:keys [data voice]}]
                                       (a/>!! event-chan data))))]
        (schedule! v)
        (is (= '({:index 0, :elapsed-ms 0, :dur 1}
                 {:index 1, :elapsed-ms 10N, :dur 2}
                 {:index 2, :elapsed-ms 30N, :dur 1}
                 {:index 3, :elapsed-ms 40N, :dur 2}
                 {:index 4, :elapsed-ms 60N, :dur 1}
                 {:index 5, :elapsed-ms 70N, :dur 1})
               (mapv #(-> %
                          (select-keys [:index :elapsed-ms])
                          (assoc :dur (get-dur %)))
                     (a/<!! result-chan))))))

    (testing "`:dur-ms` and `:dur` values are present and correct"
      (let [{:keys [event-chan result-chan]} (async-events-tester
                                              default-continue)
            v (atom (assoc base-voice
                           :started-at (+ 1 (now))
                           :on-event (fn [{:keys [data _voice _dur _dur-ms]}]
                                       #_(println (keys data))
                                       (a/>!! event-chan (select-keys
                                                          data
                                                          [:index :dur :dur-ms])))))]
        (schedule! v)
        (is (= [{:index 0, :dur 1, :dur-ms 10N}
                {:index 1, :dur 2, :dur-ms 20N}
                {:index 2, :dur 1, :dur-ms 10N}
                {:index 3, :dur 2, :dur-ms 20N}
                {:index 4, :dur 1, :dur-ms 10N}
                {:index 5, :dur 1, :dur-ms 10N}]
               (a/<!! result-chan)))))

    (testing "`:durs` can be a function and `:dur-ms` and `:dur` values are present and correct"
      (let [total-events 5
            {:keys [event-chan result-chan]} (async-events-tester
                                              (fn [ev _]
                                                (println "continue")
                                                (< (ev :index) total-events)))
            v (atom {})
            _ (reset! v (assoc base-voice
                               :loop? true
                               :durs (fn [{:keys [index ratio] :as _voice}]
                                       (* ratio (wrap-at index [1 2 1 2 1 1])))
                               :started-at (+ 1 (now))
                               :on-event (fn [{:keys [data _voice _dur _dur-ms]}]
                                           (println (keys data))
                                           (a/>!! event-chan (select-keys
                                                              data
                                                              [:index :dur :dur-ms]))
                                           (when (> (:index data) total-events)
                                             (swap! v assoc
                                                    :loop? false
                                                    :playing? false)))))]
        (schedule! v)
        (is (= [{:index 0, :dur 1, :dur-ms 10N}
                {:index 1, :dur 2, :dur-ms 20N}
                {:index 2, :dur 1, :dur-ms 10N}
                {:index 3, :dur 2, :dur-ms 20N}
                {:index 4, :dur 1, :dur-ms 10N}
                {:index 5, :dur 1, :dur-ms 10N}]
               (a/<!! result-chan)))))

    (testing "The assoc'ed value in `:on-event`, `:event-at`, which is a timestamp, occurs very close or on the expected interval. NOTE The interval from the last event onset to it's offset is missing. "
      (let [{:keys [event-chan result-chan]} (async-events-tester
                                              default-continue)
            v (atom (assoc base-voice
                           :started-at (+ 1 (now))
                           :on-event (fn [{:keys [data voice]}]
                                       (a/>!! event-chan
                                              (assoc data :event-at (now))))))]
        (schedule! v)
        (is (= true
               (->> (a/<!! result-chan)
                    (map :event-at)
                    get-time-interval
                    (map (fn [test-interval real-interval]
                           (close-to test-interval 7 real-interval))
                         [10 20 10 20 10])
                    (every? true?))))))))
