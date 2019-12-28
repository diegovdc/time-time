(ns nanc-in-a-can.taller-abierto.internal)

(defn arg-count [f]
  {:pre [(instance? clojure.lang.AFunction f)]}
  (-> f class .getDeclaredMethods first .getParameterTypes alength))

(defn validate [spec input]
  (or (s/valid? spec input)
      (throw (ex-info (s/explain-str spec input) {:input input}))))
