(ns time-time.dynacan.players.gen-poly
  (:require [time-time.sequencing-3 :as s]
            [taoensso.timbre :as log]))
(comment
  ;; [[durs]]
;;;  ref-voice
;;; find-first sirve para durs distintas

  (gen-poly [{:r 1 :durs [1 2 3 4] :ref? true}
             {:r 1 :durs [3 4] :cp 2}
             {:r 2 :durs [1 2 3 4] :cp 1}])

  (def v1 (s/play! [1] (fn [_] (log/info _))
                   :loop? true
                   :ratio 1/4))

  (swap! v1 assoc :playing? false)

  #_(def ref-voice (play! [1 2 3 5 6]))

  (def voice-state {:durs [1],
                    :index 2              ;; sucede en: + started-at elapsed 750
                    :started-at 1597195400418
                    :current-event {:dur-ms 250N, :dur 1/4}
                    :playing? true
                    :ratio 1/4
                    :loop? true
                    :tempo 60
                    :elapsed 500N} )


  (add-to v1 :id 1 2 (fn [_] (println "segunda voz")))

  cp @ v1 (- 5 2)



  (defn add-to [ref-voice id cp ratio on-event])

  [4 7]


  (def second-voice (add-to! ref-voice :id :v1 :cp-from (rand-int 4) :durs [1 2 3 4] :loop? false))

  (def third-voice (add-to! ref-voice :cp 3 :durs [1 2 3 4]))

  (hush! :v1)
  )
