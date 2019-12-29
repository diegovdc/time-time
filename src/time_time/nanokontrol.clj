(ns time-time.nanokontrol
  (:require [overtone.live :refer [on-event remove-event-handler]]))


(def nanokontrol (atom {:f0 {:val 0 :action println}
                        :k0 {:val 0 :action println}
                        :f1 {:val 0 :action println}
                        :k1 {:val 0 :action println}
                        :f2 {:val 0 :action println}
                        :k2 {:val 0 :action println}
                        :f3 {:val 0 :action println}
                        :k3 {:val 0 :action println}
                        :f4 {:val 0 :action println}
                        :k4 {:val 0 :action println}
                        :f5 {:val 0 :action println}
                        :k5 {:val 0 :action println}
                        :f6 {:val 0 :action println}
                        :k6 {:val 0 :action println}
                        :f7 {:val 0 :action println}
                        :k7 {:val 0 :action println}}))


(def fader-channels {0 :f0
                     1 :f1
                     2 :f2
                     3 :f3
                     4 :f4
                     5 :f5
                     6 :f6
                     7 :f7})

(def knob-channels {16 :k0
                    17 :k1
                    18 :k2
                    19 :k3
                    20 :k4
                    21 :k5
                    22 :k6
                    23 :k7})

(defn no-action-warning [fader]
  (fn [_] (println (str  "No action specified for fader: " fader))))

(defn get-action [state-atom event-fader]
    (get-in @state-atom
            [event-fader :action]
            (no-action-warning event-fader)))

(defn setup-fader-listeners
  ([state-atom] (setup-fader-listeners state-atom ::faders))
  ([state-atom listener-ns-keyword]
   (on-event [:midi :pitch-bend]
             (fn [e]
               (when-let [event-fader (fader-channels (:channel e))]
                 (swap! state-atom
                        #(update-in %
                                    [event-fader :val]
                                    (constantly (:note e))))
                 ((get-action state-atom event-fader) (:note e))))
             listener-ns-keyword)))

(defn setup-knob-listeners
  ([state-atom] (setup-knob-listeners state-atom ::knobs))
  ([state-atom listener-ns-keyword]
   (on-event [:midi :control-change]
            (fn [e]
              (when-let [channel (knob-channels (:note e))]
                (swap! state-atom
                       #(update-in %
                                   [channel :val]
                                   (fn [val]
                                     (if (#{2 63} (:data2 e))
                                       ;; min and max keep within midi range
                                       (min 127 (max 0 (inc val)))
                                       (max 0 (min 127 (dec val)))))))
                ((get-action state-atom channel) (-> @state-atom channel :val))))
            ::knobs)))



(defn set-action* [controller fader action]
  (swap! controller #(assoc-in % [fader :action] action)))


(def set-action (partial set-action* nanokontrol))

(defn start! []
  (setup-fader-listeners nanokontrol)
  (setup-knob-listeners nanokontrol))


(defn stop! []
  (remove-event-handler ::faders)
  (remove-event-handler ::knobs))


(start!)
