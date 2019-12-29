(ns taller-abierto.internal
  (:require [clojure.spec.alpha :as s]))

(defn arg-count [f]
  {:pre [(instance? clojure.lang.AFunction f)]}
  (-> f class .getDeclaredMethods first .getParameterTypes alength))

(defn validate [spec input]
  (or (s/valid? spec input)
      (throw (ex-info (s/explain-str spec input) {:input input}))))
