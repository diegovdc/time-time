(ns user
  (:require
   [clojure.pprint]
   [clojure.tools.namespace.repl :refer [set-refresh-dirs refresh refresh-all]]
   [taoensso.timbre :as log]
   [potemkin :refer [import-vars]]
   [clj-utils.core]))

(log/set-level! :info)
(set-refresh-dirs "src" "test")

(import-vars
 [clj-utils.core
  data
  clear-data
  spy
  spy->
  capture
  capture-all
  tap
  tap->
  tc
  tca])

(defn connect []
  (eval '(do (require '[overtone.core :as o])
             (o/boot-external-server))))

(defn test-sound []
  (eval '(do (require '[overtone.core :refer :all])
             (demo (sin-osc)))))
