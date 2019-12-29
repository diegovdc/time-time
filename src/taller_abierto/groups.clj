(ns taller-abierto.groups
  (:require [overtone.core :refer :all]))

(defonce main-g (group "get-on-the-bus main"))
(defonce early-g (group "early birds" :head main-g))
(defonce later-g (group "latecomers" :after early-g))
