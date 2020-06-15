(ns time-time.utils.core)

(defn get-time-interval
  "Calculate time interval between pairs of timestamps from a list of timestamps"
  [times]
  (->> times
       (partition 2 1)
       (map (fn [[a b]] (- b a)))))

(get-time-interval [0 100 200 300])
(100 100 100)

(defn close-to [test-num by num]
  (and (>= num (- test-num by))
       (<= num (+ test-num by))))

(close-to 100 5 100)
(close-to 100 5 101)
(close-to 100 5 105)
(close-to 100 5 106)
(close-to 100 5 99)
(close-to 100 5 95)
(close-to 100 5 94)
