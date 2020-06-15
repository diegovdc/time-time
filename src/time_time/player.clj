(ns time-time.player)

(defprotocol player
  (loop? [this] "If a canon should loop?")
  (stop! [this] "stop a canon")
  (data [this] "return the data"))
