(ns time-time.utils.async
  (:require [clojure.core.async :as a]
            [taoensso.timbre :as log]))

(defn async-events-tester [keep-running-test-fn]
    (let [event-chan (a/chan)
          result-chan (a/chan)]
      (a/go-loop [i 0 events []]
        (log/debug "[`schedule!` test] Waiting for event:" i)
        (let [event (a/<! event-chan)]
          (if (keep-running-test-fn event i)
            (do (log/debug event)
                (recur (inc i)
                       (conj events event)))
            (do
              (log/debug event)
              (log/debug "[`schedule!` test] Closing event-chan")
              (a/close! event-chan)
              (a/>! result-chan (conj events event))))))
      {:event-chan event-chan
       :result-chan result-chan}))


(comment
  "Timeout example"
  (defn <!!?
    "Reads from chan synchronously, waiting for a given maximum of milliseconds.
  If the value does not come in during that period, returns :timed-out. If
  milliseconds is not given, a default of 1000 is used."
    ([chan]
     (<!!? chan 1000))
    ([chan milliseconds]
     (let [timeout (a/timeout milliseconds)
           [value port] (a/alts!! [chan timeout])]
       (if (= chan port)
         value
         :timed-out)))))
