(ns time-time.dynacan.players.gen-poly-tests
  (:require [time-time.dynacan.players.dirt-samples :refer :all]
            [time-time.standard :refer [wrap-at]]
            [time-time.dynacan.players.gen-poly :refer [ref-rain on-event]]))

(ref-rain :id ::v1
          :durs [1/5]
          :on-event (on-event ((wrap-at index [bd ab])))
          :loop? true)

(ref-rain :id ::v2
          :ref ::v1
          :durs [1 2/3 1/6 1/6]
          :on-event (on-event (when (= 0 (wrap-at index [0 1 1 ]))
                                ((wrap-at index [ ab bass ab])
                                 (wrap-at index [0 0 4 0 6]))))
          :ratio 1/6
          :loop? true)


(stop)
