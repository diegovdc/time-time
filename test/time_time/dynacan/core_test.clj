(ns time-time.dynacan.core-test
  (:require [clojure.pprint :refer [pprint]]
            [clojure.test :refer [deftest is testing]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.properties :as prop]
            [time-time.dynacan.core
             :refer
             [find-first-event-using-cp
              get-event-at
              get-next-event
              get-next-n-events
              normalize-dur]]
            [time-time.standard :refer [wrap-at]]
            [time-time.utils.core :refer [gen-cp gen-durs gen-ratio]]))

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
             (get-event-at 1 [1 3] 3 1 ))))))

(comment (get-event-at 1 [1 2] 3)
         (get-event-at 1 [1 3] 3)
         (get-event-at 1 [1 3] 3 nil 0))

;; 1 2 1 2 1 2 1 3 1 3 1 3 1                               cp
;; -----------------,. 121212

;;; TODO find-first-event-using-cp with negative `cp`

(do
  (defn modulo-cp
    "Finds the `cp` in modulo form for given the
  `current-index` and the `echoic-distance-event-qty`"
    [ref-durs ref-current-index echoic-distance-event-qty]
    (mod (+ ref-current-index echoic-distance-event-qty)
         (count ref-durs)))
  (modulo-cp [1 3] 6 3))

(do
  (defn ensure-voice-cp
    [current-ref-index ref-durs echoic-distance-qty voice-durs voice-cp]
    (if (and (= ref-durs voice-durs) (nil? voice-cp))
      (modulo-cp ref-durs current-ref-index echoic-distance-qty)
      (or voice-cp echoic-distance-qty)))

  (ensure-voice-cp 1 [1 2] 3 [2 5] nil))

(defn find-relative-voice-first-event
  [echoic-distance-qty
   ref-voice
   {:keys [durs cp ratio] :as rel-voice}]
  (let [current-ref-index (:index ref-voice)
        ref-durs (:durs ref-voice)
        rel-voice-cp (ensure-voice-cp current-ref-index ref-durs echoic-distance-qty durs cp)
        ref-start-index (mod current-ref-index (count ref-durs))
        cp-event (get-event-at (:ratio ref-voice)
                               ref-durs
                               echoic-distance-qty
                               ref-start-index)]
    (find-first-event-using-cp ratio
                               durs
                               rel-voice-cp
                               (:elapsed cp-event))))

(find-first-event-using-cp 1/2
                           [1 4]
                           0
                           7)



(mod 7 (count [0 1 2 3 4 5]))

(let [first-part (get-next-n-events [1 2] {:echoic-distance 10 :elapsed 0 :index 0 :ratio 1} 6)
      new-durs [1 3]
      second-voice-durs [1 3 5 6]
      second-part (get-next-n-events new-durs (last first-part) 7)
      v1 (map #(dissoc % :original-dur "echoic-distnace")
              (concat first-part  (drop 1 second-part)))
      now-v1-index 7
      now-v1 (nth v1 now-v1-index)
      echoic-distance-qty 3
      v2-cp (ensure-voice-cp now-v1-index new-durs echoic-distance-qty second-voice-durs 2)
      #_(if (= new-durs second-voice-durs #_ "TODO (and v2-cp not defined)")
          (modulo-cp new-durs now-v1-index echoic-distance-qty)
          echoic-distance-qty ;; TODO IMPROVE
          )
      cp-event (get-event-at 1 [1 3] echoic-distance-qty 1)
      second-ratio 1/2
      v2-first-event (find-first-event-using-cp second-ratio
                                                second-voice-durs
                                                v2-cp
                                                (:elapsed cp-event))]
  #_(is (=
         (:elapsed (nth v1 (+ now-v1-index cp)))
         (+ (now-v1 :elapsed) (:elapsed
                               (last
                                (get-next-n-events new-durs v2-first-event
                                                   (v2-first-event :echoic-distance-event-qty)))))))
  [now-v1
   (nth v1 (+ now-v1-index echoic-distance-qty))
   (last (get-next-n-events second-voice-durs v2-first-event (v2-first-event :echoic-distance-event-qty)))]
  #_v1)



(comment (add-to! v1 :durs [1 3 4] :cp 3 :ref-cp 10))

(deftest find-first-event-using-cp-test
  #_"Non generative, human readable tests"
  (testing ""
    (find-first-event-using-cp 1/2
                               [1 3 2]
                               3
                               7))
  (let [first-part (get-next-n-events [1 2] {:echoic-distance 10 :elapsed 0 :index 0 :ratio 1} 6)
        new-durs [1 3]
        second-voice-durs [1 3 5 6]
        second-part (get-next-n-events new-durs (last first-part) 7)
        v1 (map #(dissoc % :original-dur "echoic-distnace")
                (concat first-part  (drop 1 second-part)))
        now-v1-index 7
        now-v1 (nth v1 now-v1-index)
        echoic-distance-qty 3
        v2-cp (ensure-voice-cp now-v1-index new-durs echoic-distance-qty second-voice-durs 2)
        #_(if (= new-durs second-voice-durs #_ "TODO (and v2-cp not defined)")
            (modulo-cp new-durs now-v1-index echoic-distance-qty)
            echoic-distance-qty ;; TODO IMPROVE
            )
        cp-event (get-event-at 1 [1 3] echoic-distance-qty 1)
        second-ratio 1/2
        v2-first-event (find-first-event-using-cp second-ratio
                                                  second-voice-durs
                                                  v2-cp
                                                  (:elapsed cp-event))]
    #_(is (=
           (:elapsed (nth v1 (+ now-v1-index cp)))
           (+ (now-v1 :elapsed) (:elapsed
                                 (last
                                  (get-next-n-events new-durs v2-first-event
                                                     (v2-first-event :echoic-distance-event-qty)))))))
    [now-v1
     (nth v1 (+ now-v1-index echoic-distance-qty))
     (last (get-next-n-events second-voice-durs v2-first-event (v2-first-event :echoic-distance-event-qty)))]
    #_v1)  (testing "Ratio is twice as slow so the voice would start @ `index` 1, with `elapsed` 0, so`cp` ocurrs on index 2."
    (let [cp 2
          ref-ratio 1
          ratio 2
          durs [1 1 1]
          cp-elapsed-at (:elapsed (get-event-at ref-ratio durs cp))]
      (is (= {:ratio 2,
              :elapsed 0,
              :index 1,
              :cp 2,
              :cp-at 2,
              :echoic-distance 2,
              :echoic-distance-event-qty 1}
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
              :cp-at 3,
              :echoic-distance 0,
              :echoic-distance-event-qty 0}
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
              :cp-at 3,
              :echoic-distance 3N,
              :echoic-distance-event-qty 3N}
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
              :cp-at 3,
              :echoic-distance 3/2,
              :echoic-distance-event-qty 2}
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
                :cp-at 1,
                :echoic-distance 1N,
                :echoic-distance-event-qty 1N}
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
                :cp-at 1,
                :echoic-distance 2/3,
                :echoic-distance-event-qty 1N}
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
                                                    (first-event :echoic-distance-event-qty)))
            ref-voice-first-event (find-first-event-using-cp* ref-ratio ref-durs)
            second-voice-first-event (find-first-event-using-cp* second-ratio second-durs)
            ref-voice-events (get-next-n-events* ref-durs ref-voice-first-event)
            second-voice-events (get-next-n-events* second-durs second-voice-first-event)]
        (is (apply = (map (comp (juxt :elapsed :echoic-distance) last)
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
                                              :echoic-distance
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
              '({:index 0, :echoic-distance 8, :elapsed 0, :original-dur 2}
                {:index 1, :echoic-distance 6, :elapsed 2, :original-dur 2}
                {:index 2, :echoic-distance 4, :elapsed 4, :original-dur 4}
                {:index 3, :echoic-distance 0, :elapsed 8, :original-dur 1}
                {:index 4, :echoic-distance -1, :elapsed 9, :original-dur 2}),
              :sub-voice-events
              '({:index 3, :echoic-distance 6N, :elapsed 2N, :original-dur 1}
                {:index 4, :echoic-distance 16/3, :elapsed 8/3, :original-dur 2}
                {:index 5, :echoic-distance 4N, :elapsed 4N, :original-dur 2}
                {:index 6, :echoic-distance 8/3, :elapsed 16/3, :original-dur 4}
                {:index 7, :echoic-distance 0N, :elapsed 8N, :original-dur 1})})))
    (testing "Convergence point (`cp`) falls at `:index` 3 of `ref-voice`, with  `:original-dur` equaling 1, and with 8 units of `:elapsed` time. NOTE that indexes in different voices will not necessarily be the same, but the `:original-dur` (i.e. the nth index at `durs`) will be the same, as will be the `:elapsed` time units, and the `:echoic-distance`. Therefore any player implementation should be capable of respecting this results")
    (is (= {:elapsed 8 :original-dur 1 :echoic-distance 0}
           (-> canonic-sequence :ref-voice-events (nth 3)
               (select-keys [:elapsed :original-dur :echoic-distance]))
           (-> canonic-sequence :sub-voice-events (nth 4)
               (select-keys [:elapsed :original-dur :echoic-distance]))))))

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
       (and (= 0 (cp-event :echoic-distance))
            (= 0 (cp-event :echoic-distance-event-qty)))))))

#_(let [                                ;; NOTE, first two lines of `let` are fo
        first-part (get-next-n-events [1 2] {:echoic-distance 10 :elapsed 0 :index 0 :ratio 1} 6)
        second-part (get-next-n-events new-durs (last first-part) 7)
        ref-voice-events (map #(dissoc % :original-dur)
                              (concat first-part  (drop 1 second-part)))
        ref-voice-current-event (nth ref-voice-events current-ref-index)

        second-voice-first-event (find-relative-voice-first-event
                                  echoic-distance-qty
                                  ref-voice-current-event
                                  {:durs second-voice-durs
                                   :ratio rel-voice-ratio
                                   :cp cp})
        second-voice-at-cp-event (last (user/spy (get-next-n-events
                                                  second-voice-durs
                                                  second-voice-first-event
                                                  (second-voice-first-event
                                                   :echoic-distance-event-qty))))
        ref-voice-cp-event (last (get-next-n-events new-durs
                                                    ref-voice-current-event
                                                    echoic-distance-qty))
        ]
    [(= (wrap-at cp second-voice-durs) (:original-dur second-voice-at-cp-event))
     (= 0 (:echoic-distance second-voice-at-cp-event))
     (= (:elapsed second-voice-at-cp-event)
        (- (:elapsed ref-voice-cp-event)
           (:elapsed ref-voice-current-event)))])


(defn test-find-relative-voice-first-event
  [ref-voice-durs current-ref-index echoic-distance-qty second-voice-durs cp rel-voice-ratio]
  (let [ref-voice-events (get-next-n-events ref-voice-durs
                                            {:echoic-distance 10
                                             :elapsed 0
                                             :index 0
                                             :ratio 1}
                                            current-ref-index)
        ref-voice-current-event (last ref-voice-events)

        second-voice-first-event (find-relative-voice-first-event
                                  echoic-distance-qty
                                  ref-voice-current-event
                                  {:durs second-voice-durs
                                   :ratio rel-voice-ratio
                                   :cp cp})

        second-voice-at-cp-event (last (get-next-n-events
                                        second-voice-durs
                                        second-voice-first-event
                                        (second-voice-first-event
                                         :echoic-distance-event-qty)))

        ref-voice-cp-event (last (get-next-n-events ref-voice-durs
                                                    ref-voice-current-event
                                                    echoic-distance-qty))
        ]
    {:dur-at-cp-is-correct?
     (= (wrap-at cp second-voice-durs) (:original-dur second-voice-at-cp-event))
     :echoic-distance-is-0-at-cp (:echoic-distance second-voice-at-cp-event)
     :elapsed-at-cp-is-equal-for-rel-and-ref-voices?
     (= (:elapsed second-voice-at-cp-event)
        (- (:elapsed ref-voice-cp-event)
           (:elapsed ref-voice-current-event)))}))

(-> @user/data :ff)
(find-first-event-using-cp 1 [2 1] 1 1)
(-> @user/data :rel)
(apply find-first-event-using-cp (-> @user/data :ff))
(apply find-relative-voice-first-event (:rel @user/data))
(comment
  (test-find-relative-voice-first-event [1] 1 1 [1 1 1] 2 1)

  ;; FIXME esto errorea
;;; Estas variaciones funcionan
  (test-find-relative-voice-first-event [1] 1 1 [2 1] 2 1)
  (test-find-relative-voice-first-event [1] 1 1 [2 1] 0 1)
  (test-find-relative-voice-first-event [1] 1 1 [1 2] 1 1)

  ;; FIXME
  (test-find-relative-voice-first-event  [2] 1 3 [1 2 1] 5 3 ))

(defspec find-relative-voice-first-event-prop-test 50
  (prop/for-all
   [new-durs (gen-durs)
    current-ref-index (gen-cp)
    echoic-distance-qty (gen-cp)
    second-voice-durs (gen-durs)
    cp (gen-cp)
    rel-voice-ratio (gen-ratio)]
   (= {:dur-at-cp-is-correct? true
       :echoic-distance-is-0-at-cp 0
       :elapsed-at-cp-is-equal-for-rel-and-ref-voices? true}

      (test-find-relative-voice-first-event new-durs
                                            current-ref-index
                                            echoic-distance-qty
                                            second-voice-durs
                                            cp
                                            rel-voice-ratio))))
