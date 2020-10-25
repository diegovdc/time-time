(ns time-time.web
  (:require ["tone/build/esm/index.js" :as Tone]
            [clojure.set :as set]
            [clojure.walk :as walk]
            [reagent.core :as r]
            [reagent.dom :as dom]
            [taoensso.timbre :as timbre :include-macros true]
            [time-time.dynacan.players.gen-poly :refer [ref-rain]]
            [time-time.sequencing-3 :as sequencing-3]))

(defn start-tone []
  (-> (Tone/start)
      (.then (fn [_]
               (let [status (.-state sequencing-3/transport)]
                 (timbre/info (str "Sequencing transport function status: " status)))))))

(defn start-button []
  [:button
   {:on-click start-tone}
   "Start"])

(defn init-api []
  (set! (.. js/window -Tone) Tone)
  (set! (.. js/window -refrain)
        (fn [opts]
          (apply ref-rain
                 (apply concat
                        (-> opts js->clj
                            walk/keywordize-keys
                            (set/rename-keys {:onEvent :on-event
                                              :loop :loop?})
                            (update :id (comp keyword str))))))))

(defn ^:export init [node-id]
  (init-api)
  (start-tone)
  (when node-id
    (dom/render [start-button] (js/document.getElementById node-id)))
  (js/console.log "hello from time tiem"))

(defn start []
  (init "app")
  (js/console.log "starting") )

(defn stop []
  (js/console.log "stoping") )

(comment
  (def synth (.toDestination (Tone/Synth.)))
  (.triggerAttackRelease synth "C4" "8n"))
