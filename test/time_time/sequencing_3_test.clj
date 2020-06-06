(ns time-time.sequencing-3-test
  (:require [time-time.sequencing-3 :refer [calculate-next-voice-state
                                            play-event? schedule?]]
            [clojure.test :refer [deftest testing is]]))



(deftest calculate-next-voice-state-test
  (testing "Increases the `index` by one, updates the `elapsed` value and updates (or sets) the `current-event-dur`"
    (is (=
         {:durs [1 2 1],
          :index 1,
          :tempo 60,
          :loop? false,
          :started-at 0,
          :elapsed 1000,
          :ratio 1,
          :current-event-dur 1000}
         (calculate-next-voice-state {:durs [1 2 1]
                                      :index 0
                                      :tempo 60
                                      :loop? false
                                      :started-at 0
                                      :elapsed 0
                                      :ratio 1})))
    (is (=
         {:durs [1 2 1],
          :index 2,
          :tempo 60,
          :loop? false,
          :started-at 0,
          :elapsed 3000,
          :ratio 1,
          :current-event-dur 2000}
         (calculate-next-voice-state {:durs [1 2 1],
                                      :index 1,
                                      :tempo 60,
                                      :loop? false,
                                      :started-at 0,
                                      :elapsed 1000,
                                      :ratio 1,
                                      :current-event-dur 1000}))))
  (testing "`current-event-dur` is related to tempo"
    (is (= 500
           (:current-event-dur
            (calculate-next-voice-state {:durs [1 2 1]
                                         :index 0
                                         :tempo 120
                                         :loop? false
                                         :started-at 0
                                         :elapsed 0
                                         :ratio 1}))))
    (is (= 1000
           (:current-event-dur
            (calculate-next-voice-state {:durs [1 2 1]
                                         :index 1
                                         :tempo 120
                                         :loop? false
                                         :started-at 0
                                         :elapsed 0
                                         :ratio 1}))))
    (is (= 4000
           (:current-event-dur
            (calculate-next-voice-state {:durs [1 2 1]
                                         :index 1
                                         :tempo 30
                                         :loop? false
                                         :started-at 0
                                         :elapsed 0
                                         :ratio 1}))))))

(deftest play-event?-test
  (testing "Play a first and second event but not a third nor fourth"
    (is (true? (play-event? {:index 0 :durs [1 2]})))
    (is (true? (play-event? {:index 1 :durs [1 2]})))
    (is (nil? (play-event? {:index 2 :durs [1 2]})))
    (is (nil? (play-event? {:index 3 :durs [1 2]}))))
  (testing "Loop playing (always play events)"
    (is (true? (play-event? {:index 0 :durs [1 2] :loop? true})))
    (is (true? (play-event? {:index 1 :durs [1 2] :loop? true})))
    (is (true? (play-event? {:index 2 :durs [1 2] :loop? true})))
    (is (true? (play-event? {:index 3 :durs [1 2] :loop? true})))))

(deftest schedule?-test
  (testing "Schedule a first and second event but not a third nor fourth"
    (is (true? (schedule? {:index 0 :durs [1 2]})))
    (is (true? (schedule? {:index 1 :durs [1 2]})))
    (is (nil? (schedule? {:index 2 :durs [1 2]})))
    (is (nil? (schedule? {:index 3 :durs [1 2]}))))
  (testing "Loop scheduling (always schedule events)"
    (is (true? (schedule? {:index 0 :durs [1 2] :loop? true})))
    (is (true? (schedule? {:index 1 :durs [1 2] :loop? true})))
    (is (true? (schedule? {:index 2 :durs [1 2] :loop? true})))
    (is (true? (schedule? {:index 3 :durs [1 2] :loop? true})))))
