(ns time-time.dynacan.players.refrain.v2-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [time-time.dynacan.players.refrain.v2 :refer [ref-rain refrains]]
   [time-time.sequencing-4 :as s4]))

(deftest ref-rain-test
  (with-redefs [refrains (atom {})
                s4/schedule! (fn [_voice])]
    (ref-rain :id :hola
              :durs [1]
              :loop? false
              :on-event println)
    (testing "Creates a refrain with some config keys"
      (is (= #{:before-update
               :current-event
               :cycle
               :cycle-0-index
               :cycle-delta
               :cycle-len
               :durs
               :elapsed-dur
               :elapsed-ms
               :id
               :index
               :loop?
               :new-cycle?
               :on-event
               :on-schedule
               :on-startup-error
               :playing?
               :ratio
               :ref
               :started-at
               :tempo
               :refrain/config}
             (set (keys @(:hola @refrains))))))
    (testing "The `:refrain/config` key only contains the specified keys"
      (is (= {:id :hola,
              :durs [1],
              :loop? false,
              :on-event println}
             (:refrain/config @(:hola @refrains)))))
    (testing "The refrain `:id` is a top level key"
      (is (= :hola (:id @(:hola @refrains)))))))
