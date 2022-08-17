(ns time-time.utils.core
  (:require [clojure.spec.gen.alpha :as gen]))

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

(defn get-next-n-events [durs voice n]
  (loop [n* n
         index (:index voice)
         res []]
    (if (= -1 n*)
      res
      (recur (dec n*)
             (inc index)
             (conj res
                   (merge
                    voice
                    (let [original-dur (nth durs (mod index (count durs)))
                          dur (* (voice :ratio) original-dur)
                          edq (:events-from-cp (last res))]
                      {:durs durs
                       :index index
                       :dur dur
                       :original-dur original-dur
                       :elapsed (+ (get (last res) :dur 0)
                                   (get (last res)
                                        :elapsed
                                        (:elapsed voice)))
                       :events-from-cp (if edq
                                         (dec edq)
                                         (:events-from-cp voice))
                       :interval-from-cp (if (empty? res)
                                           (voice :interval-from-cp)
                                           (- (-> res
                                                  last
                                                  :interval-from-cp)
                                              (-> res last :dur)))})))))))

(comment
  ;;test
  (get-next-n-events [1 1 1 1]
                     {:interval-from-cp 4 :elapsed 0 :index 0 :ratio 1 :events-from-cp 4}
                     4))

(defn get-next-event [voice durs]
  (merge voice {:index (inc (:index voice))
                :dur (* (voice :ratio)
                        (nth durs (mod (voice :index) (count durs))))
                :elapsed (+ (get voice :dur 0)
                            (get voice
                                 :elapsed
                                 (:elapsed voice)))}))
