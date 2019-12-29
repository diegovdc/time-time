(ns nanc-in-a-can.taller-abierto.graphs.logic.core)

(defn get-next-node [history graph]
  (let [prev-node (or (last history)
                      (rand-nth (keys graph)))]
    (rand-nth (seq (graph prev-node)))))

(defn play-next!
  [state graph]
  (let [{:keys [history]} @state
        next-node (get-next-node history graph)]
    (println "a sonar:" (-> next-node meta :name))
    (swap! state update :history conj next-node)))

(comment
  (defn some-synth
    [instruments config]
    (println "playing" instruments))

  (def node-a {:instruments [:audio-1 :audio-2]
               :synth #'some-synth})

  (def node-b {:instruments [:audio-1 :audio-2]
               :synth #'some-synth})

  (def graph {#'node-a #{#'node-b}
              #'node-b #{#'node-a}})

  (def graph-state (atom {:history []
                          :status :playing
                          :config {}}))
  (play-next! graph-state graph))
