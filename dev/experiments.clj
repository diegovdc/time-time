(ns experiments)

(comment
  (def kick (freesound-sample 2086))
  (def hh (sample (freesound-path 44937)))
  (def grey-whale (sample (freesound-path 413377)))
  (def whales-1 (freesound-sample 322539))
  (def nome (metronome 200))
  (definst kick* [] (pan2 (play-buf:ar 1 kick :rate 0.7)))
  (definst whales-1* [pan 0] (pan2 (play-buf:ar 1 whales-1) pan))
  (kick*)
  (kick)
  (def w1 (whales-1*))
  (ctl w1 :pan 1)
  (ctl w2 :pan -1)

  (node-pause* w1)
  (node-start* w2))
