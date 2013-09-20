(ns cljs_ajax_battleship.scratch-server
  (:require [cljs.repl.browser]))

;; first
;; (require 'cljs.repl.browser)

(defn start-cljs-repl []
  (cemerick.piggieback/cljs-repl
   :repl-env (doto (cljs.repl.browser/repl-env :port 9000)
               cljs.repl/-setup)))
