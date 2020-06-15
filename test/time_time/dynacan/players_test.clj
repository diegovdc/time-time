(ns time-time.dynacan.players-test
  (:require
   [overtone.core :refer [now]]
   [time-time.dynacan.players :refer [std!]]
   [time-time.utils.async :refer [async-events-tester]]
   [time-time.utils.core :refer [close-to get-time-interval]]
   [clojure.test :refer [testing deftest is]]
   [clojure.core.async :as a]))

(deftest std!-test
  (let [end-of-canon (fn [ev _]
                       ;; last event of first (slowest) voice
                       (not= ((juxt :voice :index) ev) [0 4]))]
    (testing "`cp` for all three voices happens at the same time"
      (let [{:keys [event-chan result-chan]} (async-events-tester end-of-canon)]
        (std! [1 1 1 3 4] [1 2 4] 3
              (fn [{:keys [data]}] (a/>!! event-chan data))
              :tempo 6000)
        (is (apply = (->> (a/<!! result-chan)
                          (filter #(-> % :index (= 3)))
                          (map (juxt :started-at :elapsed))
                          (map #(apply + %)))))))

    (testing "In practice (using `:event-at`), the `cp`s happen very close to each other (less than 2ms)"
      (let [{:keys [event-chan result-chan]} (async-events-tester end-of-canon)]
        (std! [1 1 1 3 4] [1 2 4 7 9] 3
              (fn [{:keys [data]}] (a/>!! event-chan (assoc data :event-at (now))))
              :tempo 6000)
        (is (> 2 (->> (a/<!! result-chan)
                      (filter #(-> % :index (= 3)))
                      (map :event-at)
                      sort
                      get-time-interval
                      last)))))

    (testing "`canon-dur` for all voices is the same"
      (let [{:keys [event-chan result-chan]} (async-events-tester end-of-canon)]
        (std! [1 1 1 3 4] [1 2 4 7 9] 3
              (fn [{:keys [data]}] (a/>!! event-chan data))
              :tempo 6000)
        (is (apply = (->> (a/<!! result-chan)
                          (filter #(-> % :index (= 0)))
                          (map :canon-dur))))))

    (testing "`total-dur` for all voices is different"
      (let [{:keys [event-chan result-chan]} (async-events-tester end-of-canon)]
        (std! [1 1 1 3 4] [1 2 4 7 9] 3
              (fn [{:keys [data]}] (a/>!! event-chan data))
              :tempo 6000)
        (is (= 5 (->> (a/<!! result-chan)
                      (filter #(-> % :index (= 0)))
                      (map :total-dur)
                      set
                      count)))))

    (testing "for all voices (+ start-delay total-dur rest) equals `canon-dur`"
      (let [{:keys [event-chan result-chan]} (async-events-tester end-of-canon)
            _ (std! [1 1 1 3 4] [1 2 4 7 9] 3
                    (fn [{:keys [data]}] (a/>!! event-chan data))
                    :tempo 6000)
            result (a/<!! result-chan)]
        (is (apply =
                   (-> result first :canon-dur)
                   (->> result
                        (filter #(-> % :index (= 0)))
                        (map (juxt :start-delay :total-dur :rest))
                        (map (partial apply +)))))))))
