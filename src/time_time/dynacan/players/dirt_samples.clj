(ns time-time.dynacan.players.dirt-samples
  (:require [clojure.java.io :as io]
            [overtone.core :as o]
            [clojure.string :as str]))

(def ^:private samples
  (->> (clojure.java.io/file "/home/diego/code/Dirt-Samples")
       (file-seq)
       (filter #(str/includes? % ".wav"))
       (map #(-> % .getPath))
       (group-by #(first (take-last 2 (str/split % #"/"))))
       (map (fn [[k v]] [k (sort v)]))
       (into {})))

(def load-sample* (memoize (fn [path] (o/load-sample path))))

(o/defsynth sample-synth
  [sample 0, pan 0, start-pos 0, rate 1, amp 1]
  (o/out 0 (-> (o/play-buf:ar 1 sample :start-pos start-pos :rate rate :action o/FREE)
               (* amp)
               (o/pan2 pan))))

(defmacro make-sample [sample-name]
  (list 'def (symbol sample-name)
        (list 'fn
              (list [] (list (symbol sample-name) 0))
              (list [(symbol "sample-num")]
                    `(sample-synth
                      (load-sample*
                       (nth (get samples ~sample-name)
                            ~(symbol "sample-num")
                            (first (get samples ~sample-name)))))))))

(defmacro make-sample-fns [samples]
  (mapv #(list 'make-sample %) samples))

(make-sample-fns ["jungle" "blue" "padlong" "rm" "d" "foo" "erk" "flick" "birds" "space" "ab" "armora" "drumtraks" "jungbass" "rs" "bleep" "glasstap" "chin" "ht" "made" "pad" "diphone2" "lt" "alex" "f" "cb" "e" "realclaps" "ades3" "off" "sugar" "sequential" "crow" "stab" "mouth" "speakspell" "cp" "blip" "glitch2" "pebbles" "bottle" "tok" "kicklinn" "msg" "hoover" "circus" "sheffield" "gtr" "mute" "reverbkick" "amencutup" "yeah" "mp3" "fm" "procshort" "sundance" "speech" "ravemono" "sn" "glitch" "v" "tabla2" "tablex" "gabbaloud" "noise" "hand" "ulgab" "hmm" "sax" "ifdrums" "cr" "hh27" "breath" "wobble" "rave" "co" "click" "birds3" "hardcore" "short" "perc" "tacscan" "jazz" "em2" "casio" "stomp" "made2" "bev" "koy" "incoming" "battles" "bassfoo" "can" "speedupdown" "dork2" "invaders" "bass3" "rave2" "toys" "lighter" "bd" "if*" "psr" "arp" "trump" "auto" "speechless" "clubkick" "latibro" "bubble" "metal" "arpy" "baa" "sf" "peri" "sitar" "bin" "kurt" "bend" "monsterb" "subroc3d" "voodoo" "industrial" "techno" "gab" "control" "juno" "mt" "gretsch" "hh" "fest" "less" "mash2" "diphone" "hardkick" "coins" "insect" "jvbass" "numbers" "bass" "dr55" "hit" "hc" "ul" "alphabet" "db" "noise2" "print" "uxay" "bass2" "tabla" "tink" "tech" "outdoor" "feelfx" "xmas" "bass0" "drum" "ho" "notes" "moog" "sd" "baa2" "dorkbot" "dist" "h" "gabba" "proc" "clak" "feel" "mash" "future" "odx" "wind" "bass1" "miniyeah" "linnhats" "popkick" "ades4" "gabbalouder" "electro1" "sine" "newnotes" "seawolf" "breaks125" "haw" "ades2" "cosmicg" "pluck" "led" "east" "oc" "ade" "cc" "world" "house" "fire" "sid"])
