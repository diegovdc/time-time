(ns time-time.dynacan.players.std-test
  (:require [clojure.core.async :as a]
            [clojure.test :refer [deftest is testing]]
            [time-time.standard :refer [now]]
            [taoensso.timbre :as log]
            [time-time.dynacan.players.std :refer [std!]]
            [time-time.player :as p :refer [player]]
            [time-time.utils.async :refer [async-events-tester]]
            [time-time.utils.core :refer [close-to get-time-interval]]))

(declare get-cp-events)

(deftest std!-test
  (let [end-of-canon (fn [ev _]
                       ;; last event of first (slowest) voice
                       (not= ((juxt :voice :index) ev) [0 4]))]
    (testing "`cp` for all three voices happens at the same time"
      (let [{:keys [event-chan result-chan]} (async-events-tester end-of-canon)]
        (std! [1 1 1 3 4] [1 2 4] 3
              (fn [{:keys [data]}] (a/>!! event-chan data))
              :tempo 600)
        (is (apply = (->> (a/<!! result-chan)
                          (filter #(-> % :index (= 3)))
                          (map (juxt :started-at :elapsed))
                          (map #(apply + %)))))))

    (testing "In practice (using `:event-at`), the `cp`s happen very close to each other (less than 2ms)"
      (let [{:keys [event-chan result-chan]} (async-events-tester end-of-canon)]
        (std! [1 1 1 3 4] [1 2 4 7 9] 3
              (fn [{:keys [data]}] (a/>!! event-chan (assoc data :event-at (now))))
              :tempo 600)
        (is (> 10 (->> (a/<!! result-chan)
                       (filter #(-> % :index (= 3)))
                       (map :event-at)
                       sort
                       get-time-interval
                       last)))))
;;;;;;;;;

    #_(testing "`canon-dur` for all voices is the same"
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
              :tempo 300)
        (is (= 5 (->> (a/<!! result-chan)
                      (filter #(-> % :index (= 0)))
                      (mapv :total-dur)
                      set
                      count)))))

    (testing "for all voices (+ start-delay total-dur rest) equals `canon-dur`"
      (let [{:keys [event-chan result-chan]} (async-events-tester end-of-canon)
            _ (std! [1 1 1 3 4] [1 2 4 7 9] 3
                    (fn [{:keys [data]}] (a/>!! event-chan data))
                    :tempo 300)
            result (a/<!! result-chan)]
        (is (apply =
                   (-> result first :canon-dur)
                   (->> result
                        (filter #(-> % :index (= 0)))
                        (map (juxt :start-delay :total-dur :rest))
                        (map (partial apply +)))))))

    (testing "Voices loop correctly, respecting the rest interval"
      (let [durs (->> (range (+ 2 (rand-int 3)))
                      (mapv #(+ % 1 (rand-int 3))))
            voices (->> (range (+ 2 (rand-int 5)))
                        (mapv #(+ 1 % (rand-int 10))))
            cp (rand-int (count durs))
            end-index (* 3 (count durs))
            {:keys [event-chan result-chan]}
            (async-events-tester
             (fn [ev _]
               (not= ((juxt :voice :index) ev) [0 end-index])))
            canon (std! durs voices cp
                        (fn [{:keys [data]}] (a/>!! event-chan (assoc data :interval (- (now) (:started-at data)))))
                        :tempo 600
                        :loop? true)
            result (a/<!! result-chan)]
        (p/stop! canon)
        (is (= true
               (every? true?
                       (->> result
                            (group-by :voice)
                            (mapv (comp
                                   (partial get-cp-events durs cp)
                                   (partial map :interval)
                                   val))
                            (apply mapv (fn [& args]
                                          (let [min* (apply min args)
                                                max* (apply max args)
                                                avg (/ (+ min* max*) 2)]
                                            (close-to avg 10 max*))))))))))))


(defn get-cp-events [durs cp values]
  (let [len (count durs)]
    (->> values
         (map-indexed vector)
         (filter #(= cp (mod (first %) len)))
         (map second))))

(deftest get-cp-events-test
  (is (= (get-cp-events (range 4) 2 (range 20))
         '(2 6 10 14 18))))
