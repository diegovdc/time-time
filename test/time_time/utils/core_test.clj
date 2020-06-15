(ns time-time.utils.core-test
  (:require
   [clojure.test :refer [deftest testing is]]
   [time-time.utils.core :refer [get-time-interval close-to]]))

(deftest get-time-interval-test
  (is (= (get-time-interval [0 100 200 300])
         '(100 100 100))))

(deftest close-to-test
  (is (true? (close-to 100 5 100)))
  (is (true? (close-to 100 5 101)))
  (is (true? (close-to 100 5 105)))
  (is (false? (close-to 100 5 106)))
  (is (true? (close-to 100 5 99)))
  (is (true? (close-to 100 5 95)))
  (is (false? (close-to 100 5 94))))
