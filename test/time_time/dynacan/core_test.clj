(ns time-time.dynacan.core-test
  (:require [clojure.pprint :refer [pprint]]
            [clojure.test :refer [deftest is testing]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.properties :as prop]
            [time-time.dynacan.core
             :refer
             [ensure-rel-voice-cp
              find-first-event-using-cp
              find-relative-voice-first-event
              get-event-at
              modulo-cp
              normalize-dur]]
            [time-time.standard :refer [wrap-at]]
            [time-time.utils.core
             :refer
             [gen-cp gen-durs gen-ratio get-next-event get-next-n-events]]))

(comment (require '[user :refer [spy capture]]))

(deftest get-event-at-test
  (let [durs [1 2 2]]
    (testing "Get event within first cycle"
      (testing "Ratio of 1"
        (is (= {:current-dur 2 :elapsed 1}
               (get-event-at 1 durs 1))))
      (testing "Ratio of 1/2"
        (is (= {:current-dur 1 :elapsed 1/2}
               (get-event-at 1/2 durs 1))))
      (testing "Ratio of 3"
        (is (= {:current-dur 6 :elapsed 9}
               (get-event-at 3 durs 2)))))
    (testing "Get event in a cycle other than the first"
      (is (= {:current-dur 1/2 :elapsed 5}
             (get-event-at 1/2 durs 6))))
    (testing "Can calculate the event at starting on an index other than `0`"
      (is (= {:current-dur 1 :elapsed 7}
             (get-event-at 1 [1 3] 3 1))))))

(deftest find-first-event-using-cp-test
  #_"Non generative, human readable tests"
  (testing "Ratio is twice as slow so the voice would start @ `index` 1, with `elapsed` 0, so`cp` ocurrs on index 2."
    (let [cp 2
          ref-ratio 1
          ratio 2
          durs [1 1 1]
          cp-elapsed-at (:elapsed (get-event-at ref-ratio durs cp))]
      (is (= {:ratio 2,
              :elapsed 0,
              :index 1,
              :cp 2,
              :cp-elapsed 2,
              :interval-from-cp 2,
              :events-from-cp 1}
             (find-first-event-using-cp ratio durs cp cp-elapsed-at)))))

  (testing "Voice would start at index 2 directly on cp"
    (let [ref-ratio 1
          ratio 2
          cp 2
          durs [1 2 3]
          cp-elapsed-at (:elapsed (get-event-at ref-ratio durs cp))]
      (is (= {:ratio 2,
              :elapsed 3,
              :index 2,
              :cp 2,
              :cp-elapsed 3,
              :interval-from-cp 0,
              :events-from-cp 0}
             (find-first-event-using-cp ratio durs cp cp-elapsed-at)))))
  (testing "Voice would start at `index` 2 with `:elapsed` 0. So durations are (+ 1 1.5 0.5) which equals 3. So index 2 will fall on cp."
    (let [ref-ratio 1
          ratio 1/2
          cp 2
          durs [1 2 3]
          cp-elapsed-at (:elapsed (get-event-at ref-ratio durs cp))]
      (is (= {:ratio 1/2,
              :elapsed 0N,
              :index 2,
              :cp 2,
              :cp-elapsed 3,
              :interval-from-cp 3N,
              :events-from-cp 3N}
             (find-first-event-using-cp ratio durs cp cp-elapsed-at)))))

  (testing "Non-looping first-event. Because ratio < ref-ratio and `:loop?` is `false`, the voice would start at `index` 0 on time (`:elapsed`) 3/2. So durations are (+ 1.5 #_form-elapsed 0.5 1) which equals three. Therefore index 2 will fall on cp."
    (let [ref-ratio 1
          ratio 1/2
          cp 2
          durs [1 2 3]
          cp-elapsed-at (:elapsed (get-event-at ref-ratio durs cp))]
      (is (= {:ratio 1/2,
              :elapsed 3/2,
              :index 0,
              :cp 2,
              :cp-elapsed 3,
              :interval-from-cp 3/2,
              :events-from-cp 2}
             (find-first-event-using-cp ratio durs cp cp-elapsed-at :loop? false)))))

  (testing "Find convergence point for two different rhythmic sequences "
    (testing "Convergence on index 1 both voices starting on 0"
      (let [ref-ratio 1
            ref-durs [1 2 3]
            second-ratio 1/2
            second-durs [2 2]
            cp 1
            cp-elapsed-at (:elapsed (get-event-at ref-ratio ref-durs cp))]
        (is (= {:ratio 1/2,
                :elapsed 0N,
                :index 0,
                :cp 1,
                :cp-elapsed 1,
                :interval-from-cp 1N,
                :events-from-cp 1N}
               (find-first-event-using-cp second-ratio second-durs cp cp-elapsed-at :loop? false)))))
    (testing "Convergence on index 1, second voice starting slightly late"
      (let [ref-ratio 1
            ref-durs [1 2 3]
            second-ratio 1/3
            second-durs [2 2]
            cp 1
            cp-elapsed-at (:elapsed (get-event-at ref-ratio ref-durs cp))]
        (is (= {:ratio 1/3,
                :elapsed 1/3,
                :index 0,
                :cp 1,
                :cp-elapsed 1,
                :interval-from-cp 2/3,
                :events-from-cp 1N}
               (find-first-event-using-cp second-ratio second-durs cp cp-elapsed-at :loop? false)))))
    (testing "Convergence on index 10"
      (let [ref-ratio (+ 1 (rand-int 6))
            ref-durs [1 2 3]
            second-ratio (/ 1 (+ 1 (rand-int 7)))
            second-durs [1 2]
            cp 10
            cp-elapsed-at (:elapsed (get-event-at ref-ratio ref-durs cp))
            find-first-event-using-cp* (fn [ratio durs]
                                         (find-first-event-using-cp ratio
                                                                    durs
                                                                    cp
                                                                    cp-elapsed-at
                                                                    :loop? true))
            get-next-n-events* (fn [durs first-event]
                                 (get-next-n-events durs first-event
                                                    (first-event :events-from-cp)))
            ref-voice-first-event (find-first-event-using-cp* ref-ratio ref-durs)
            second-voice-first-event (find-first-event-using-cp* second-ratio second-durs)
            ref-voice-events (get-next-n-events* ref-durs ref-voice-first-event)
            second-voice-events (get-next-n-events* second-durs second-voice-first-event)]
        (is (apply = (map (comp (juxt :elapsed :interval-from-cp) last)
                          [ref-voice-events
                           second-voice-events])))))))

(comment
  "IMPORTANT TEST.
   It will make clear how the system works at the present time. Please read the
   `testing` descriptions. The property based tests that follow will also test
   the same things (with other data) but in a less explicit manner.")
(deftest temporal-canon-sequencing-data-using-get-event-at-and-find-first-event-using-cp
  #_"Non generative, human readable tests"
  (let [durs [2 2 4 1]
        reference-ratio 1
        subordinate-ratio 2/3
        cp 3
        cp-elapsed-at (:elapsed (get-event-at reference-ratio durs cp))
        reference-first-event (find-first-event-using-cp
                               reference-ratio
                               durs
                               cp
                               cp-elapsed-at)
        subordinate-first-event (find-first-event-using-cp
                                 subordinate-ratio
                                 durs
                                 cp
                                 cp-elapsed-at)
        ;; first events can also be considered here as `voices`
        ;; (they contain the necessary data)
        reference-voice reference-first-event
        subordinate-voice subordinate-first-event
        simplify-data (fn [voice-events]
                        (map #(select-keys % [:index
                                              :interval-from-cp
                                              :elapsed
                                              :original-dur])
                             voice-events))
        reference-voice-events (simplify-data
                                (get-next-n-events durs reference-voice 4))
        subordinate-voice-events (simplify-data
                                  (get-next-n-events durs subordinate-voice 4))
        canonic-sequence {:ref-voice-events reference-voice-events
                          :sub-voice-events subordinate-voice-events}]
    (testing "How a canonic sequence of events should look like (map keys are just for reference). NOTE sub-voices may start somewhere other that index 0 of `durs`, in fact they will start as close as possible to `:elapsed` time equaling `0`"
      (is (= canonic-sequence
             {:ref-voice-events
              '({:index 0, :interval-from-cp 8, :elapsed 0, :original-dur 2}
                {:index 1, :interval-from-cp 6, :elapsed 2, :original-dur 2}
                {:index 2, :interval-from-cp 4, :elapsed 4, :original-dur 4}
                {:index 3, :interval-from-cp 0, :elapsed 8, :original-dur 1}
                {:index 4, :interval-from-cp -1, :elapsed 9, :original-dur 2}),
              :sub-voice-events
              '({:index 3, :interval-from-cp 6N, :elapsed 2N, :original-dur 1}
                {:index 4, :interval-from-cp 16/3, :elapsed 8/3, :original-dur 2}
                {:index 5, :interval-from-cp 4N, :elapsed 4N, :original-dur 2}
                {:index 6, :interval-from-cp 8/3, :elapsed 16/3, :original-dur 4}
                {:index 7, :interval-from-cp 0N, :elapsed 8N, :original-dur 1})})))
    (testing "Convergence point (`cp`) falls at `:index` 3 of `ref-voice`, with  `:original-dur` equaling 1, and with 8 units of `:elapsed` time. NOTE that indexes in different voices will not necessarily be the same, but the `:original-dur` (i.e. the nth index at `durs`) will be the same, as will be the `:elapsed` time units, and the `:interval-from-cp`. Therefore any player implementation should be capable of respecting this results")
    (is (= {:elapsed 8 :original-dur 1 :interval-from-cp 0}
           (-> canonic-sequence :ref-voice-events (nth 3)
               (select-keys [:elapsed :original-dur :interval-from-cp]))
           (-> canonic-sequence :sub-voice-events (nth 4)
               (select-keys [:elapsed :original-dur :interval-from-cp]))))))

(defn test-existence-of-cp-on-the-same-duration-for-different-voices
  "Demonstrate that the event at `cp` in two different voices occurs at the same
  time each voices is a map with keys: `:ratio` `:elapsed-at`,
  `:index`, `elapsed-at` and `:index` for both voices are calculated with
  `find-first-event-using-cp` and `get-event-at`.

  IMPORTANT: Note that `get-event-at` always uses the `reference-ratio`. This
  should be the same for all voices in the canon.

  The function `get-next-events` is used as means for the demonstration,
  see comment with example use."

  [reference-ratio ratio-2 durs cp loop?]
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
                                cp-elapsed-at
                                :loop? loop?))
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

(defspec prop-find-first-event-using-cp 100
  (testing "That for any two ratios find-first-event-using-cp can find a
           cp where both voices will coincide on the same duration at the
           same time (the duration is of course scaled to the ratio)."
    (prop/for-all
     [ratio-1 (gen-ratio)
      ratio-2 (gen-ratio)
      durs (gen-durs)
      cp (gen-cp)]
     (test-existence-of-cp-on-the-same-duration-for-different-voices
      ratio-1 ratio-2 durs cp
      #_:loop? (rand-nth [true false])))))

(defspec prop-events-from-cp 100
  (testing "Echoic distance event quantity is correctly calculated,
           so that if we iterate a voice starting on the first event
           (as returned by `find-first-event-using-cp`)
           by the number of events in `:events-from-cp`
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
                            :events-from-cp)))
           cp-event-elapsed (cp-event :elapsed)]
       (= cp-elapsed-at cp-event-elapsed)))))

(defspec prop-interval-from-cp 100
  (testing "Echoic distance is correctly calculated,
           so that if we iterate a voice starting on the first event
           (as returned by `find-first-event-using-cp`)
           by the number of events in `:events-from-cp`
           (returned by the same function)
           then on the cp-event we the `:interval-from-cp` will be
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
                            :events-from-cp)))]
       (and (= 0 (cp-event :interval-from-cp))
            (= 0 (cp-event :events-from-cp)))))))

(defn test-find-relative-voice-first-event
  [ref-voice-durs current-ref-index interval-from-cp-qty second-voice-durs cp rel-voice-ratio loop?]
  (let [ref-voice-events (get-next-n-events ref-voice-durs
                                            {:interval-from-cp 10
                                             :elapsed 0
                                             :index 0
                                             :ratio 1}
                                            current-ref-index)
        ref-voice-current-event (last ref-voice-events)

        second-voice-first-event (find-relative-voice-first-event
                                  interval-from-cp-qty
                                  ref-voice-current-event
                                  {:durs second-voice-durs
                                   :ratio rel-voice-ratio
                                   :cp cp
                                   :loop? loop?})

        second-voice-at-cp-event (last (get-next-n-events
                                        second-voice-durs
                                        second-voice-first-event
                                        (second-voice-first-event
                                         :events-from-cp)))

        ref-voice-cp-event (last (get-next-n-events ref-voice-durs
                                                    ref-voice-current-event
                                                    interval-from-cp-qty))]
    {:dur-at-cp-is-correct?
     (= (wrap-at cp second-voice-durs) (:original-dur second-voice-at-cp-event))
     :interval-from-cp-is-0-at-cp (:interval-from-cp second-voice-at-cp-event)
     :elapsed-at-cp-is-equal-for-rel-and-ref-voices?
     (= (:elapsed second-voice-at-cp-event)
        (- (:elapsed ref-voice-cp-event)
           (:elapsed ref-voice-current-event)))}))

(defspec find-relative-voice-first-event-prop-test 50
  (prop/for-all
   [new-durs (gen-durs)
    current-ref-index (gen-cp)
    interval-from-cp-qty (gen-cp)
    second-voice-durs (gen-durs)
    cp (gen-cp)
    rel-voice-ratio (gen-ratio)]
   (= {:dur-at-cp-is-correct? true
       :interval-from-cp-is-0-at-cp 0
       :elapsed-at-cp-is-equal-for-rel-and-ref-voices? true}

      (test-find-relative-voice-first-event new-durs
                                            current-ref-index
                                            interval-from-cp-qty
                                            second-voice-durs
                                            cp
                                            rel-voice-ratio
                                            (rand-nth [true false])))))

(deftest modulo-cp-test
  (testing "Starting at index `6` find the cp index (in the durs vector) at (+ 6 3)"
    (is (= 1 (modulo-cp [1 3] 6 3)))))

(deftest ensure-rel-voice-cp-test
  (testing "Because `rel-voice-durs` is equal to `ref-durs` and `rel-voice-cp`
   is `nil`, then the cp should be 0 (3 events in modulo form calculated from index
   1 of `ref-durs`"
    (is (= 0 (ensure-rel-voice-cp 1 [1 2] 3 [1 2] nil))))
  (testing "Because `rel-voice-durs` is equal to `ref-durs` and `rel-voice-cp`
   is 1, then the cp should be 1"
    (is (= 1 (ensure-rel-voice-cp 1 [1 2] 3 [1 2] 1))))
  (testing "Because `rel-voice-durs` is not equal to `ref-durs` and `rel-voice-cp` is `nil`, then the cp should be the same as the `events-from-cp`"
    (is (= 3 (ensure-rel-voice-cp 1 [1 2] 3 [2 5] nil)))))
