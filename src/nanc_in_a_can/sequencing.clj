(ns nanc-in-a-can.sequencing
  (:use [overtone.live]))
; setup a sound for our metronome to use
(def kick (sample (freesound-path 2086)))
(def hh (sample (freesound-path 44937)))

(def nome (metronome 200))

(do
  (defn sequencer- [nome sequence* on-event state]
    (let [{:keys [start-at index repeat]} @state
          val* (first sequence*)]
      (at (nome (+ start-at (:elapsed val*))) 
          (on-event val* index)
          (if-not (:remainder? (user/spy val*))
            (apply-by (nome (+ start-at (:elapsed (second sequence*)))) 
                      sequencer- 
                      nome
                      (rest sequence*)
                      on-event 
                      [(do (swap! state #(update-in % [:index] inc))
                           state)])
            (if (not= 0 repeat)
              (apply-by (nome (+ start-at (:elapsed val*) (:dur val*))) 
                        sequencer- 
                        nome
                        (@state :sequence)
                        on-event 
                        [(do (swap! state 
                                    #(-> %
                                         (assoc :start-at 
                                                (+ start-at (:elapsed val*) (:dur val*)))
                                         (update-in [:index] 
                                                    (constantly 0))
                                         (update-in [:repeat] 
                                                    (fn [r] (if (= r :inf) r (dec r))))))
                             state)])))))
    state)
  
  (defn sequencer2
    ([nome sequence on-event] (sequencer- nome sequence on-event (atom {:on-event on-event
                                                                        :sequence sequence
                                                                        :start-at (nome)
                                                                        :index 0})))
    ([nome sequence on-event {:keys [repeat offset] :or {offset 0} :as opts
                              }] 
     (sequencer- nome sequence on-event (atom (merge opts 
                                                      {:on-event on-event
                                                       :sequence sequence
                                                       :start-at (+ (nome) offset)
                                                       :index 0})))))
    
  (let [nome* (metronome 60)
        now- (now)]
    ((user/capture :sequencer) (sequencer2 
                                nome 
                                (conj (mapv #(hash-map :elapsed %) (range 5)) 
                                      {:remainder? true :dur 1 :elapsed 5})  
                                (fn [vals index]
                                  (swap! quil-test/st #(assoc % :bg (rand 255)))
                                  (hh) 
                                  nil) 
                                {:repeat :inf}
                                )))
 )

(comment
  (-> @user/data :sequencer (swap! #(update-in % [:repeat] (constantly nil))))
  (stop)
  (kill-server))

(comment (overtone.at-at/stop-and-reset-pool! my-pool))

(comment (overtone.sc.server/boot-server))


(comment
  (->> @user/data :canon
       (map (fn [voice] (sequencer2 
                        nome 
                        voice
                        (fn [vals index]
                          (if (= 0 (mod index 2))
                            (swap! quil-test/st #(assoc % :bg (rand 255)))
                            (swap! quil-test/st #(assoc % :radius (rand 3.0)))
                            )
                          ((rand-nth [kick hh])) 
                          nil) 
                        {:repeat 10}
                        )))))


 
(comment                                      
  (recording-start "~/Desktop/foo.wav")
  (recording-stop))
