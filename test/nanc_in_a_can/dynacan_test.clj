(ns nanc-in-a-can.dynacan-test
  (:require [clojure.pprint :refer [pprint]]
            [clojure.spec.gen.alpha :as gen]
            [clojure.test :refer [deftest is testing]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.properties :as prop]
            [time-time.dynacan
             :refer
             [find-first-event-using-cp
              get-event-at
              get-next-event
              get-next-n-events
              normalize-dur]]))

(comment (require '[user :refer [spy]]))

(deftest get-event-at-test
  (let [durs [1 2 2]]
    (testing "Get event within first cycle"
      (testing "Ratio of 1"
        (is (= (get-event-at 1 durs 1)
               {:current-dur 2 :elapsed 1})))
      (testing "Ratio of 1/2"
        (is (= (get-event-at 1/2 durs 1)
               {:current-dur 1 :elapsed 1/2})))
      (testing "Ratio of 3"
        (is (= (get-event-at 3 durs 2)
               {:current-dur 6 :elapsed 9}))))
    (testing "Get event in a cycle other than the first"
      (testing "Ratio of 1/2"
        (is (= (get-event-at 1/2 durs 6)
               {:current-dur 1/2 :elapsed 5}))))))


(defn test-existence-of-cp-with-on-the-same-duration
  "Demonstrate that the event at `cp` in two different voices occurs at the same
  time each voices is a map with keys: `:ratio` `:elapsed-at`,
  `:index`, `elapsed-at` and `:index` for both voices are calculated with
  `find-first-event-using-cp` and `get-event-at`.

  IMPORTANT: Note that `get-event-at` always uses the `reference-ratio`. This
  should be the same for all voices in the canon.

  The function `get-next-events` is used as means for the demonstration,
  see comment with example use."

  [reference-ratio ratio-2 durs cp]
  (let [cp-elapsed-at (:elapsed (get-event-at reference-ratio durs cp))
        main-voice (merge {:ratio reference-ratio}
                          (find-first-event-using-cp
                           reference-ratio
                           durs
                           cp
                           cp-elapsed-at))
        secondary-voice (merge {:ratio ratio-2}
                               (find-first-event-using-cp
                                ratio-2
                                durs
                                cp
                                cp-elapsed-at))
        ;; mv = main-voice
        ;; sv = secondary-voice
        mv-event (last (get-next-n-events durs main-voice cp))
        mv-elapsed-at-cp (:elapsed mv-event)
        result (loop [sv-event (get-next-event secondary-voice durs)]
                 (let [sv-elapsed (get sv-event :elapsed 0)
                       mv-dur (normalize-dur mv-event)
                       sv-dur (normalize-dur sv-event)
                       both-same-duration (= mv-dur sv-dur)]
                   (cond
                     (and (= mv-elapsed-at-cp sv-elapsed)
                          both-same-duration)
                     ,,,[true mv-event sv-event :same-duration]
                     (= mv-elapsed-at-cp sv-elapsed)
                     ,,,[false durs mv-event sv-event
                         {:mv-dur mv-dur
                          :sv-dur sv-dur} :different-duration]
                     (< mv-elapsed-at-cp sv-elapsed)
                     ,,,[false durs mv-event sv-event]
                     :else
                     (recur (get-next-event sv-event durs)))))]
    (or (first result)
        (do (pprint result)
            false))))

(defn ->positive>0 [n]
  (cond (= 0 n) 1
        (> 0 n) (* -1 n)
        :else n))

(defn gen-ratio []
  (gen/fmap ->positive>0 (gen/ratio)))

(defn gen-durs []
  (gen/not-empty (gen/vector (gen/fmap ->positive>0 (gen/int)))))

(defn gen-cp []
  (gen/fmap ->positive>0 (gen/int)))

(defspec prop-find-first-event-using-cp 100
  (testing "That for any two ratios find-first-event-using-cp can find a
           cp where both voices will coincide on the same duration at the
           same time (the duration is of course scaled to the ratio)."
    (prop/for-all
     [ratio-1 (gen-ratio)
      ratio-2 (gen-ratio)
      durs (gen-durs)
      cp (gen-cp)]
     (test-existence-of-cp-with-on-the-same-duration
      ratio-1
      ratio-2
      durs
      cp))))


(defspec prop-echoic-distance-event-qty 100
  (testing "Echoic distance event quantity is correctly calculated,
           so that if we iterate a voice starting on the first event
           (as returned by `find-first-event-using-cp`)
           by the number of events in `:echoic-distance-event-qty`
           (returned by the same function)
           we will arrive exactly at the cp."
    (prop/for-all
     [ratio (gen-ratio)
      durs (gen-durs)
      cp (gen-cp)]
     (let [cp-elapsed-at (:elapsed (get-event-at ratio durs cp))
           first-event (find-first-event-using-cp
                        ratio
                        durs
                        cp
                        cp-elapsed-at)

           cp-event (last (get-next-n-events
                           durs
                           first-event
                           (first-event
                            :echoic-distance-event-qty)))
           cp-event-elapsed (cp-event :elapsed)]
       (= cp-elapsed-at cp-event-elapsed)))))

(defspec prop-echoic-distance 100
  (testing "Echoic distance is correctly calculated,
           so that if we iterate a voice starting on the first event
           (as returned by `find-first-event-using-cp`)
           by the number of events in `:echoic-distance-event-qty`
           (returned by the same function)
           then on the cp-event we the `:echoic-distance` will be
           exactly 0."
    (prop/for-all
     [ratio (gen-ratio)
      durs (gen-durs)
      cp (gen-cp)]
     (let [cp-elapsed-at (:elapsed (get-event-at ratio durs cp))
           first-event (find-first-event-using-cp
                        ratio
                        durs
                        cp
                        cp-elapsed-at)

           cp-event (last (get-next-n-events
                           durs
                           first-event
                           (first-event
                            :echoic-distance-event-qty)))]
       (= 0 (cp-event :echoic-distance))))))
