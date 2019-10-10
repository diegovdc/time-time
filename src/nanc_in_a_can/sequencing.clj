(ns nanc-in-a-can.sequencing
  (:use [overtone.live]))

(defn sequencer- [nome sequence* on-event state]
  (let [{:keys [start-at index repeat stop?]} @state
        val* (first sequence*)]
    (when (not stop?)
      (at (nome (+ start-at (:elapsed val*))) 
          (on-event val* index))
      (if-not (:remainder? (user/spy :mute val*))
        (let [next-event
              (apply-at (nome (+ start-at (:elapsed (second sequence*)))) 
                        sequencer- 
                        nome
                        (rest sequence*)
                        on-event 
                        [(do (swap! state #(update-in % [:index] inc))
                             state)])]
          (swap! state #(assoc % :next-event next-event)))
        (if (not= 0 repeat)
          (apply-at (nome (+ start-at (:elapsed val*) (:dur val*))) 
                    sequencer- 
                    nome
                    (@state :sequence)
                    (@state :on-event) 
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
  
(defn sequencer
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
(comment  
  (let [nome* (metronome 60)
        now- (now)]
    ((user/capture :sequencer) (sequencer
                                nome 
                                (conj (mapv #(hash-map :elapsed %) (range 5)) 
                                      {:remainder? true :dur 1 :elapsed 5})  
                                (fn [vals index]
                                        ;(swap! quil-test/st #(assoc % :bg (rand 255)))
                                  (hh) 
                                  nil) 
                                {:repeat :inf}
                                ))))
 

(comment
  (-> @user/data :sequencer (swap! #(update-in % [:repeat] (constantly nil))))
  (stop)
  (kill-server))

(comment (overtone.at-at/stop-and-reset-pool! my-pool))

(comment (overtone.sc.server/boot-server))


(comment
  (->> @user/data :canon
       (map (fn [voice] (sequencer 
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















;;;;;;;;;;;;;;;;;;;;;; v2

(do
  (def ref-voice (atom {:started-at (now)
                        :playing? true}))
  (def voice (atom {:playing? true
                    :ref-voice ref-voice
                    :next-event {:at 5000}}))
  (defn sequ [voice]
    (let [starting (-> @voice :ref-voice deref :started-at)
          next-at (-> @voice :next-event :at)] 
      (apply-at (- (+ next-at starting) 100)
                #(when (-> @voice :ref-voice deref :playing?)
                   (do
                     (apply-at (+ next-at starting) println "hola")
                     (swap! voice 
                            (fn [v-data] 
                              (update-in v-data 
                                         [:next-event :at] 
                                         (fn [t] (user/spy (+ t 1000))))))
                     (sequ voice))))))


  (sequ voice))
(swap! ref-voice #(assoc % :playing? false))
