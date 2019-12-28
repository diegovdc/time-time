(ns nanc-in-a-can.sequencing
  (:use [overtone.core]))

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
  ([nome sequence on-event]
   (sequencer- nome
               sequence
               on-event
               (atom {:on-event on-event
                      :sequence sequence
                      :start-at (nome)
                      :index 0})))
  ([nome sequence on-event {:keys [repeat offset]
                            :or {offset 0}
                            :as opts}]
   (sequencer- nome
               sequence
               on-event
               (atom (merge opts
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
  (require '[nanc-in-a-can.core :refer [converge]])
  (def kick (freesound 2086))
  (let [nome (metronome 120)]
    (->> (converge {:durs (repeat 6 1)
                    :tempos [1]
                    :cps [5]
                    :bpm 120
                    :period 1})
         (map (fn [voice] (sequencer
                          nome
                          voice
                          (fn [vals index]
                            (kick)
                            nil)
                          {:repeat nil}))))))
