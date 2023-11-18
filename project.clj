(defproject org.clojars.videco/time-time "0.1.0-SNAPSHOT"
  :description "Polytemporal sequencer"
  :url "https://github.com/diegovdc/time-time"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.10.1"]
                 [org.clojure/core.async "1.2.603"]
                 [org.clojure/test.check "0.10.0"]
                 [com.taoensso/timbre "4.10.0"]
                 ;; TODO maybe remove dependency of `overtone` in favor of `overtone.at-at`?
                 ;; This would require copying `overtone.music.time` somewhere
                 ;; because `apply-at` is very useful
                 [overtone/overtone "0.10.6"]])
