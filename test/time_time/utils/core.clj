(ns time-time.utils.core)

(defn get-time-interval
  "Calculate time interval between pairs of timestamps from a list of timestamps"
  [times]
  (->> times
       (partition 2 1)
       (map (fn [[a b]] (- b a)))))

(defn close-to [test-num by num]
  (and (>= num (- test-num by))
       (<= num (+ test-num by))))
