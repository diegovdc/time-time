(ns time-time.utils.core
  (:require [clojure.spec.gen.alpha :as gen]))

(defn rotate [a n]
  (let [l (count a)
        off (mod (+ (mod n l) l) l)]
    (concat (drop off a) (take off a))))

(defn get-time-interval
  "Calculate time interval between pairs of timestamps from a list of timestamps"
  [times]
  (->> times
       (partition 2 1)
       (map (fn [[a b]] (- b a)))))

(defn close-to [test-num by num]
  (and (>= num (- test-num by))
       (<= num (+ test-num by))))

(defn ->positive>0 [n]
  (cond (= 0 n) 1
        (> 0 n) (* -1 n)
        :else n))

(defn ->positive>x [x n]
  (cond (= x n) 1
        (> x n) (+ x (->positive>0 n))
        :else n))


(defn gen-ratio
  ([] (gen/fmap ->positive>0 (gen/ratio)))
  ([greater-than]
   (gen/fmap (partial ->positive>x greater-than) (gen/ratio))))

(defn gen-durs []
  (gen/not-empty (gen/vector (gen/fmap ->positive>0 (gen/int)))))

(defn gen-cp []
  (gen/fmap ->positive>0 (gen/int)))
