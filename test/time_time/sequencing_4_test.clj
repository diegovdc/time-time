(ns time-time.sequencing-4-test
  (:require
   [clojure.core.async :as a]
   [clojure.test :refer [deftest is testing]]
   [time-time.sequencing-4
    :refer
    [calculate-next-voice-state init-voice-data play-event? schedule!
     update-voice-state!]]
   [time-time.standard :refer [now wrap-at]]
   [time-time.utils.async :refer [async-events-tester]]
   [time-time.utils.core :refer [close-to get-time-interval]]))

(deftest calculate-next-voice-state-test
  (testing "Increases the `index` by one, updates the `elapsed` value and updates (or sets) the `current-event-dur`"
    (is (= {:durs [1 2 1],
            :cycle 0,
            :index 1,
            :started-at 0,
            :current-event
            {:durs [1 2 1],
             :cycle 0,
             :index 0,
             :start-at 0,
             :cycle-delta-index 0,
             :cycle-delta 0,
             :dur 1,
             :elapsed-dur 0,
             :new-cycle? nil,
             :cycle-0-index 0,
             :elapsed-ms 0,
             :dur-s 1,
             :dur-ms 1000,
             :cycle-len 4},
            :playing? nil,
            :cycle-delta 1/4,
            :ratio 1,
            :elapsed-dur 1,
            :new-cycle? false,
            :loop? false,
            :cycle-0-index 0,
            :elapsed-ms 1000,
            :tempo 60,
            :cycle-len 4}
           (calculate-next-voice-state {:durs [1 2 1]
                                        :cycle-len 4
                                        :cycle-0-index 0
                                        :cycle-delta 0
                                        :cycle 0
                                        :index 0
                                        :tempo 60
                                        :loop? false
                                        :started-at 0
                                        :elapsed-ms 0
                                        :elapsed-dur 0
                                        :ratio 1}))))

  (testing "`cycle` calculation"
    (let [v @(init-voice-data {:durs [1 2 1]})
          states (->> (range 10)
                      (reduce (fn [acc _i]
                                (conj acc (calculate-next-voice-state (last acc))))
                              [v])
                      (mapv (comp #(select-keys % [:index :cycle :cycle-delta :cycle-0-index])
                                  :current-event)))]
      (is (= [{:index 0 :cycle 0 :cycle-delta 0 :cycle-0-index 0}
              {:index 1 :cycle 0 :cycle-delta 1/4 :cycle-0-index 0}
              {:index 2 :cycle 0 :cycle-delta 3/4 :cycle-0-index 0}
              {:index 3 :cycle 1 :cycle-delta 0 :cycle-0-index 3}
              {:index 4 :cycle 1 :cycle-delta 1/4 :cycle-0-index 3}
              {:index 5 :cycle 1 :cycle-delta 3/4 :cycle-0-index 3}
              {:index 6 :cycle 2 :cycle-delta 0 :cycle-0-index 6}
              {:index 7 :cycle 2 :cycle-delta 1/4 :cycle-0-index 6}
              {:index 8 :cycle 2 :cycle-delta 3/4 :cycle-0-index 6}
              {:index 9 :cycle 3 :cycle-delta 0 :cycle-0-index 9}
              {:index 10 :cycle 3 :cycle-delta 1/4 :cycle-0-index 9}]
             states)))
    (testing "different ratios"
      (let [v @(init-voice-data {:durs [1 2 1]
                                 :ratio 2})
            states (->> (range 10)
                        (reduce (fn [acc _i]
                                  (conj acc (calculate-next-voice-state (last acc))))
                                [v])
                        (mapv (comp #(select-keys % [:index :cycle :cycle-delta :cycle-0-index :elapsed-dur])
                                    :current-event)))]
        (is (= [{:index 0, :cycle 0, :cycle-delta 0, :cycle-0-index 0, :elapsed-dur 0}
                {:index 1, :cycle 0, :cycle-delta 1/4, :cycle-0-index 0, :elapsed-dur 2}
                {:index 2, :cycle 0, :cycle-delta 3/4, :cycle-0-index 0, :elapsed-dur 6}
                {:index 3, :cycle 1, :cycle-delta 0, :cycle-0-index 3, :elapsed-dur 8}
                {:index 4, :cycle 1, :cycle-delta 1/4, :cycle-0-index 3, :elapsed-dur 10}
                {:index 5, :cycle 1, :cycle-delta 3/4, :cycle-0-index 3, :elapsed-dur 14}
                {:index 6, :cycle 2, :cycle-delta 0, :cycle-0-index 6, :elapsed-dur 16}
                {:index 7, :cycle 2, :cycle-delta 1/4, :cycle-0-index 6, :elapsed-dur 18}
                {:index 8, :cycle 2, :cycle-delta 3/4, :cycle-0-index 6, :elapsed-dur 22}
                {:index 9, :cycle 3, :cycle-delta 0, :cycle-0-index 9, :elapsed-dur 24}
                {:index 10, :cycle 3, :cycle-delta 1/4, :cycle-0-index 9, :elapsed-dur 26}]
               states))))
    (testing "`new-cycle?"
      (let [v @(init-voice-data {:durs [1 2 1]
                                 :ratio 2})
            states (->> (range 10)
                        (reduce (fn [acc _i]
                                  (conj acc (calculate-next-voice-state (last acc))))
                                [v])
                        (mapv (comp #(select-keys % [:index :cycle :cycle-delta :new-cycle?])
                                    :current-event)))]
        (is (= [{:index 0, :cycle 0, :cycle-delta 0, :new-cycle? true}
                {:index 1, :cycle 0, :cycle-delta 1/4, :new-cycle? false}
                {:index 2, :cycle 0, :cycle-delta 3/4, :new-cycle? false}
                {:index 3, :cycle 1, :cycle-delta 0, :new-cycle? true}
                {:index 4, :cycle 1, :cycle-delta 1/4, :new-cycle? false}
                {:index 5, :cycle 1, :cycle-delta 3/4, :new-cycle? false}
                {:index 6, :cycle 2, :cycle-delta 0, :new-cycle? true}
                {:index 7, :cycle 2, :cycle-delta 1/4, :new-cycle? false}
                {:index 8, :cycle 2, :cycle-delta 3/4, :new-cycle? false}
                {:index 9, :cycle 3, :cycle-delta 0, :new-cycle? true}
                {:index 10, :cycle 3, :cycle-delta 1/4, :new-cycle? false}]
               states))))
    (testing "different ratios and tempo"
      (let [v @(init-voice-data {:durs [1 2 1]
                                 :ratio 2
                                 :tempo 120})
            states (->> (range 10)
                        (reduce (fn [acc _i]
                                  (conj acc (calculate-next-voice-state (last acc))))
                                [v])
                        (mapv (comp #(select-keys % [:index :cycle :cycle-delta :cycle-0-index :elapsed-dur])
                                    :current-event)))]
        (is (= [{:index 0, :cycle 0, :cycle-delta 0, :cycle-0-index 0, :elapsed-dur 0}
                {:index 1, :cycle 0, :cycle-delta 1/4, :cycle-0-index 0, :elapsed-dur 2}
                {:index 2, :cycle 0, :cycle-delta 3/4, :cycle-0-index 0, :elapsed-dur 6}
                {:index 3, :cycle 1, :cycle-delta 0, :cycle-0-index 3, :elapsed-dur 8}
                {:index 4, :cycle 1, :cycle-delta 1/4, :cycle-0-index 3, :elapsed-dur 10}
                {:index 5, :cycle 1, :cycle-delta 3/4, :cycle-0-index 3, :elapsed-dur 14}
                {:index 6, :cycle 2, :cycle-delta 0, :cycle-0-index 6, :elapsed-dur 16}
                {:index 7, :cycle 2, :cycle-delta 1/4, :cycle-0-index 6, :elapsed-dur 18}
                {:index 8, :cycle 2, :cycle-delta 3/4, :cycle-0-index 6, :elapsed-dur 22}
                {:index 9, :cycle 3, :cycle-delta 0, :cycle-0-index 9, :elapsed-dur 24}
                {:index 10, :cycle 3, :cycle-delta 1/4, :cycle-0-index 9, :elapsed-dur 26}]
               states))))
    (testing "`cycle-len` is different from the `durs` sum"
      (let [v @(init-voice-data {:durs [1 2 1] :cycle-len 3})
            states (->> (range 10)
                        (reduce (fn [acc _i]
                                  (conj acc (calculate-next-voice-state (last acc))))
                                [v])
                        (mapv (comp #(select-keys % [:index :cycle :cycle-delta :cycle-0-index :elapsed-dur])
                                    :current-event)))]
        ;; when elapsed dur is a multiple of 3 then cycle-delta is 0
        (is (= [{:index 0, :cycle 0, :cycle-delta 0, :cycle-0-index 0, :elapsed-dur 0}
                {:index 1, :cycle 0, :cycle-delta 1/3, :cycle-0-index 0, :elapsed-dur 1}
                {:index 2, :cycle 1, :cycle-delta 0, :cycle-0-index 2, :elapsed-dur 3}
                {:index 3, :cycle 1, :cycle-delta 1/3, :cycle-0-index 2, :elapsed-dur 4}
                {:index 4, :cycle 1, :cycle-delta 2/3, :cycle-0-index 2, :elapsed-dur 5}
                {:index 5, :cycle 2, :cycle-delta 1/3, :cycle-0-index 2, :elapsed-dur 7}
                {:index 6, :cycle 2, :cycle-delta 2/3, :cycle-0-index 2, :elapsed-dur 8}
                {:index 7, :cycle 3, :cycle-delta 0, :cycle-0-index 7, :elapsed-dur 9}
                {:index 8, :cycle 3, :cycle-delta 2/3, :cycle-0-index 7, :elapsed-dur 11}
                {:index 9, :cycle 4, :cycle-delta 0, :cycle-0-index 9, :elapsed-dur 12}
                {:index 10, :cycle 4, :cycle-delta 1/3, :cycle-0-index 9, :elapsed-dur 13}]
               states)))))

  (testing "`current-event-dur` is related to tempo"
    (is (= 500N
           (:dur-ms (:current-event
                     (calculate-next-voice-state {:durs [1 2 1]
                                                  :cycle-len 4
                                                  :cycle-0-index 0
                                                  :cycle-delta 0
                                                  :cycle 0
                                                  :index 0
                                                  :tempo 120
                                                  :loop? false
                                                  :started-at 0
                                                  :elapsed-ms 0
                                                  :elapsed-dur 0
                                                  :ratio 1})))))
    (is (= 1000N
           (:dur-ms (:current-event
                     (calculate-next-voice-state {:durs [1 2 1]
                                                  :cycle-len 4
                                                  :cycle 0
                                                  :cycle-0-index 0
                                                  :cycle-delta 1/4
                                                  :index 1
                                                  :tempo 120
                                                  :loop? false
                                                  :started-at 0
                                                  :elapsed-ms 0
                                                  :elapsed-dur 1
                                                  :ratio 1})))))
    (is (= 4000
           (:dur-ms (:current-event
                     (calculate-next-voice-state {:durs [1 2 1]
                                                  :cycle-len 4
                                                  :cycle 0
                                                  :cycle-0-index 0
                                                  :cycle-delta 1/4
                                                  :index 1
                                                  :tempo 30
                                                  :loop? false
                                                  :started-at 0
                                                  :elapsed-ms 0
                                                  :elapsed-dur 3
                                                  :ratio 1}))))))

  (testing "`:durs` can be a function"))

(deftest update-voice-state!-test
  (testing "Basic: Operates over a voice-atom"
    (let [voice-atom (init-voice-data {:durs [1 2 1]})
          states (->> (range 10)
                      (reduce (fn [acc _i]
                                (update-voice-state! voice-atom)
                                (conj acc @voice-atom))
                              [@voice-atom])
                      (mapv (comp #(select-keys % [:index
                                                   :cycle
                                                   :cycle-delta
                                                   :cycle-0-index])
                                  :current-event)))]
      (is (= [{:index 0 :cycle 0 :cycle-delta 0 :cycle-0-index 0}
              {:index 1 :cycle 0 :cycle-delta 1/4 :cycle-0-index 0}
              {:index 2 :cycle 0 :cycle-delta 3/4 :cycle-0-index 0}
              {:index 3 :cycle 1 :cycle-delta 0 :cycle-0-index 3}
              {:index 4 :cycle 1 :cycle-delta 1/4 :cycle-0-index 3}
              {:index 5 :cycle 1 :cycle-delta 3/4 :cycle-0-index 3}
              {:index 6 :cycle 2 :cycle-delta 0 :cycle-0-index 6}
              {:index 7 :cycle 2 :cycle-delta 1/4 :cycle-0-index 6}
              {:index 8 :cycle 2 :cycle-delta 3/4 :cycle-0-index 6}
              {:index 9 :cycle 3 :cycle-delta 0 :cycle-0-index 9}
              {:index 10 :cycle 3 :cycle-delta 1/4 :cycle-0-index 9}]
             states))))
  (testing "Can operate over an update of the voice, it can update any value of the voice, e.g. `durs` and `on-event`."
    (let [voice-atom (init-voice-data {:durs [1 2 1] :on-event println})
          updated-on-event identity
          states (->> (range 10)
                      (reduce (fn [acc i]
                                (when (= 5 i)
                                  (swap! voice-atom assoc
                                         :update {:durs [3 4]
                                                  :on-event updated-on-event}))
                                (update-voice-state! voice-atom)
                                (conj acc @voice-atom))
                              [@voice-atom]))
          current-events (mapv (comp #(select-keys % [:index
                                                      :cycle
                                                      :cycle-delta
                                                      :cycle-0-index
                                                      :dur])
                                     :current-event)
                               states)
          on-events (mapv :on-event states)]
      (is (= [{:index 0, :cycle 0, :cycle-delta 0, :cycle-0-index 0, :dur 1}
              {:index 1, :cycle 0, :cycle-delta 1/4, :cycle-0-index 0, :dur 2}
              {:index 2, :cycle 0, :cycle-delta 3/4, :cycle-0-index 0, :dur 1}
              {:index 3, :cycle 1, :cycle-delta 0, :cycle-0-index 3, :dur 1}
              {:index 4, :cycle 1, :cycle-delta 1/4, :cycle-0-index 3, :dur 2}
              {:index 5, :cycle 1, :cycle-delta 3/4, :cycle-0-index 3, :dur 1}
              {:index 6, :cycle 2, :cycle-delta 0, :cycle-0-index 6, :dur 3}
              {:index 7, :cycle 2, :cycle-delta 3/4, :cycle-0-index 6, :dur 4}
              {:index 8, :cycle 3, :cycle-delta 3/4, :cycle-0-index 6, :dur 3}
              {:index 9, :cycle 4, :cycle-delta 1/2, :cycle-0-index 6, :dur 4}
              {:index 10, :cycle 5, :cycle-delta 1/2, :cycle-0-index 6, :dur 3}]
             current-events))
      (is (= (concat (repeat 6 println)
                     (repeat 5 identity))
             on-events))))
  (testing "Can update the cycle count"
    (let [voice-atom (init-voice-data {:durs [1 2 1] :on-event println})
          states (->> (range 10)
                      (reduce (fn [acc i]
                                (when (= 5 i)
                                  (swap! voice-atom assoc
                                         :update {:reset-cycle? true
                                                  :durs [3 4]}))
                                (update-voice-state! voice-atom)
                                (conj acc @voice-atom))
                              [@voice-atom]))
          current-events (mapv (comp #(select-keys % [:index
                                                      :dur
                                                      :cycle
                                                      :cycle-len
                                                      :cycle-delta
                                                      :cycle-0-index])
                                     :current-event)
                               states)]
      (is (= [{:index 0, :dur 1, :cycle 0, :cycle-len 4, :cycle-delta 0, :cycle-0-index 0}
              {:index 1, :dur 2, :cycle 0, :cycle-len 4, :cycle-delta 1/4, :cycle-0-index 0}
              {:index 2, :dur 1, :cycle 0, :cycle-len 4, :cycle-delta 3/4, :cycle-0-index 0}
              {:index 3, :dur 1, :cycle 1, :cycle-len 4, :cycle-delta 0, :cycle-0-index 3}
              {:index 4, :dur 2, :cycle 1, :cycle-len 4, :cycle-delta 1/4, :cycle-0-index 3}
              {:index 5, :dur 1, :cycle 1, :cycle-len 4, :cycle-delta 3/4, :cycle-0-index 3}
              {:index 6, :dur 3, :cycle 0, :cycle-len 7, :cycle-delta 0, :cycle-0-index 6}
              {:index 7, :dur 4, :cycle 0, :cycle-len 7, :cycle-delta 3/7, :cycle-0-index 6}
              {:index 8, :dur 3, :cycle 1, :cycle-len 7, :cycle-delta 0, :cycle-0-index 8}
              {:index 9, :dur 4, :cycle 1, :cycle-len 7, :cycle-delta 3/7, :cycle-0-index 8}
              {:index 10, :dur 3, :cycle 2, :cycle-len 7, :cycle-delta 0, :cycle-0-index 10}]
             current-events)))
    (testing "Can keep the original cycle-len"
      (let [voice-atom (init-voice-data {:durs [1 2 1] :on-event println})
            update* {:reset-cycle? true
                     :keep-cycle-len? true
                     :durs [3 4]}
            states (->> (range 10)
                        (reduce (fn [acc i]
                                  (when (= 5 i)
                                    (swap! voice-atom assoc
                                           :update update*))
                                  (update-voice-state! voice-atom)
                                  (conj acc @voice-atom))
                                [@voice-atom]))
            current-events (mapv (comp #(select-keys % [:index
                                                        :dur
                                                        :cycle
                                                        :cycle-len
                                                        :cycle-delta
                                                        :cycle-0-index
                                                        :new-cycle?])
                                       :current-event)
                                 states)]
        (is (= [{:index 0, :dur 1, :cycle 0, :cycle-len 4, :cycle-delta 0, :cycle-0-index 0, :new-cycle? true}
                {:index 1, :dur 2, :cycle 0, :cycle-len 4, :cycle-delta 1/4, :cycle-0-index 0, :new-cycle? false}
                {:index 2, :dur 1, :cycle 0, :cycle-len 4, :cycle-delta 3/4, :cycle-0-index 0, :new-cycle? false}
                {:index 3, :dur 1, :cycle 1, :cycle-len 4, :cycle-delta 0, :cycle-0-index 3, :new-cycle? true}
                {:index 4, :dur 2, :cycle 1, :cycle-len 4, :cycle-delta 1/4, :cycle-0-index 3, :new-cycle? false}
                {:index 5, :dur 1, :cycle 1, :cycle-len 4, :cycle-delta 3/4, :cycle-0-index 3, :new-cycle? false}
                {:index 6, :dur 3, :cycle 0, :cycle-len 4, :cycle-delta 0, :cycle-0-index 6, :new-cycle? true}
                {:index 7, :dur 4, :cycle 0, :cycle-len 4, :cycle-delta 3/4, :cycle-0-index 6, :new-cycle? false}
                {:index 8, :dur 3, :cycle 1, :cycle-len 4, :cycle-delta 3/4, :cycle-0-index 6, :new-cycle? true}
                {:index 9, :dur 4, :cycle 2, :cycle-len 4, :cycle-delta 1/2, :cycle-0-index 6, :new-cycle? true}
                {:index 10, :dur 3, :cycle 3, :cycle-len 4, :cycle-delta 1/2, :cycle-0-index 6, :new-cycle? true}]
               current-events))))))

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
                    :cycle-len (apply + [1 2 1 2 1 1])
                    :index 0
                    :tempo 6000 ;; NOTE a dur of `1` lasts `10N`ms
                    :loop? false
                    :started-at 0
                    :elapsed-ms 0
                    :elapsed-dur 0
                    :ratio 1
                    :playing? true}
        default-continue (fn [ev _] (< (ev :index)
                                       (dec (count (base-voice :durs)))))]
    (testing "`:elapsed-ms` values are correct"
      (let [{:keys [event-chan result-chan]} (async-events-tester
                                              default-continue)
            v (init-voice-data (assoc base-voice
                                      :started-at (+ 1 (now))
                                      :on-event (fn [{:keys [event _voice]}]
                                                  (a/>!! event-chan event))))]
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
            v (init-voice-data (assoc base-voice
                                      :started-at (+ 1 (now))
                                      :on-event (fn [{:keys [event _voice _dur _dur-ms]}]
                                                  #_(println (keys data))
                                                  (a/>!! event-chan (select-keys
                                                                     event
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
                                                (< (ev :index) total-events)))

            v (init-voice-data (assoc base-voice
                                      :loop? true
                                      :durs (fn [{:keys [index ratio] :as _voice}]
                                              (* ratio (wrap-at index [1 2 1 2 1 1])))
                                      :started-at (+ 1 (now))))
            _ (swap! v assoc :on-event (fn [{:keys [event _voice _dur _dur-ms]}]
                                         (a/>!! event-chan (select-keys
                                                            event
                                                            [:index :dur :dur-ms]))
                                         (when (> (:index event) total-events)
                                           (swap! v assoc
                                                  :loop? false
                                                  :playing? false))))]
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
            v (init-voice-data (assoc base-voice
                                      :started-at (+ 1 (now))
                                      :on-event (fn [{:keys [event voice]}]
                                                  (a/>!! event-chan
                                                         (assoc event :event-at (now))))))]
        (schedule! v)
        (is (= true
               (->> (a/<!! result-chan)
                    (map :event-at)
                    get-time-interval
                    (map (fn [test-interval real-interval]
                           (close-to test-interval 7 real-interval))
                         [10 20 10 20 10])
                    (every? true?))))))))
