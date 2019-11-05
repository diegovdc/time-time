(ns nanc-in-a-can.dynacan-test
  (:require
   [nanc-in-a-can.dynacan :refer [get-event-at
                                  find-first-event-using-cp]]
   [clojure.test :refer [deftest is testing]]))

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

(deftest find-first-event-using-cp-test
  (let [durs [1 2 1]
        cp 1
        cp-at (spy (:elapsed (get-event-at 1 durs cp)))]
    (is (= (spy (find-first-event-using-cp 3/2 durs cp cp-at))
           {:elapsed-at 1 :index 1 :real-index 1}))))
