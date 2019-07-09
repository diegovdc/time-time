(ns nanc-in-a-can.sequencing
  (:use [overtone.live]))
; setup a sound for our metronome to use
(def kick (sample (freesound-path 2086)))
(def hh (sample (freesound-path 44937)))

; setup a tempo for our metronome to use
(def one-twenty-bpm (metronome 70))

; this function will play our sound at whatever tempo we've set our metronome to 
(defn looper [nome sound]    
    (let [beat (nome)]
        (at (nome beat) (sound))
        (apply-by (nome (inc beat)) looper nome sound [])))

; turn on the metronome
(looper one-twenty-bpm kick)
(looper one-twenty-bpm hh)
(stop)

; to get a feel for how the metronome works, try defining one at the REPL
(def nome (metronome 200))
(nome)
; 8 
; why is this 8? shouldn't it be 1? let's try it again
(nome)
(nome 140)
;140
; whoah, it's almost like it's ticking in the background. 
; it is, in fact, ticking in the background. a "beat" is just convenient way to represent a timestamp.
; leave your metronome defined at the REPL, and the beat number will steadily increase, even if you aren't
; using the object for anything.
(do
  (defn sequencer [nome sound sequence]
    (at (nome (first sequence)) (sound))
    (apply-by  (nome (second sequence)) sequencer nome sound [(rest sequence)]))

  (let [nome1 (metronome 70)
        seq* (->> @user/data :canon (map #(map :elapsed %)))]
    (doseq [i (range (- (count seq*) 0))]
      (sequencer nome1 ([kick kick kick] i) (map #(+ % (nome1)) (user/spy (nth seq* i)))))
    
    )
  )

(let [n (metronome 70)]
  (n))

(def my-pool (overtone.at-at/mk-pool))



(do
  (defn sequencer- [nome sequence* on-event state]
    (user/spy "runs" state)
  (let [{:keys [start-at index loop?]} @state
        val* (first sequence*)]
    (at (nome (user/spy :quiet :at (+ start-at (:elapsed val*)))) 
        
        (on-event val* index)
        (if-not (user/spy "remainder" (:remainder? (user/spy val*)))
          (apply-by (nome (+ start-at (:elapsed (second sequence*)))) 
                    sequencer- 
                    nome
                    (rest sequence*)
                    on-event 
                    [(do (swap! state #(update-in % [:index] inc))
                         state)])
          (if loop?
            (do (println "repeating")
                (apply-by (nome (+ start-at (:elapsed val*) (:dur val*))) 
                          sequencer- 
                          nome
                          (@state :sequence)
                          on-event 
                          [(do (swap! state 
                                      #(-> %
                                           (assoc :start-at 
                                                  (+ start-at (:elapsed val*) (:dur val*)))
                                           (update-in [:index] (constantly 0))                                           
                                          ))
                               state)]))))
                                        ;my-pool
        ))
  state)
 
  (defn sequencer2
    ([nome sequence* on-event] (sequencer- nome sequence* on-event (atom {:sequence sequence*
                                                                          :start-at (nome)
                                                                          :index 0})))

    ([nome sequence* on-event opts] 
     (sequencer- nome sequence* on-event (atom (merge opts 
                                                      {:sequence sequence*
                                                       :start-at (nome)
                                                       :index 0}))))
    )
    
  (let [nome* (metronome 60)
        now- (now)]
    ((user/capture :sequencer) (sequencer2 
                                nome 
                                (conj (mapv #(hash-map :elapsed %) (range 5)) {:remainder? true :dur 1 :elapsed 5})  
                                (fn [_ __]                                    
                                  ;; (println "kicking" (+ 1 __))
                                  (hh) 
                                  nil) 
                                {:loop? true}
                                )))
 )

(-> @user/data :sequencer (swap! (constantly {:loop? false })))
(stop)
(kill-server)



(comment (overtone.at-at/stop-and-reset-pool! my-pool))
;; (at (+ 10000 (now)) #(println "hola") my-pool)
(my-pool)

(overtone.at-at/at (+ 1000 (now))
      #(println "hello from the past")
      my-pool
      :desc "Message from the past")

(comment (overtone.sc.server/boot-server))


(def a (atom 0))
(swap! a inc)


@a
