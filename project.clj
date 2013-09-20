(defproject cljs_ajax_battleship "0.1.0-SNAPSHOT"
  :description "A project done as a learning exercise while implementing the game Battleship in Clojure and ClojureScript."
  :url "http://example.com/FIXME"
  :license {:name "BSD License"
            :url "http://opensource.org/licenses/BSD-3-Clause"}

  ;; CLJ source code path
  :source-paths ["src/clj"]

  :dependencies [[cheshire "5.2.0"]
                 [com.cemerick/piggieback "0.1.0"]
                 [compojure "1.1.5"]
                 [domina "1.0.2-SNAPSHOT"]
                 [hiccups "0.2.0"]
                 [jayq "2.4.0"]
                 [org.clojure/clojurescript "0.0-1878"]
                 [shoreleave/shoreleave-remote "0.3.0"]
                 [shoreleave/shoreleave-remote-ring "0.3.0"]
                 [org.clojure/clojure "1.5.1"]]

  :plugins [;; lein-cljsbuild plugin to build a CLJS project
            [lein-cljsbuild "0.3.3"]

            ;; ring plugin
            [lein-ring "0.8.7"]]

  ;; ring tasks configuration
  :ring {:handler cljs_ajax_battleship.handler/app}

  ;; Options for clojurescript repl middleware
  :repl-options {:nrepl-middleware [cemerick.piggieback/wrap-cljs-repl]}
  
  ;; cljsbuild options configuration
  :cljsbuild {:builds
              {
               :dev
               {:source-paths ["src/cljs"]
                :compiler {:output-to "resources/public/js/cljs_ajax_battleship_dbg.js"
                           :optimizations :whitespace
                           :pretty-print true}}
               :pre-prod
               {:source-paths ["src/cljs"]
                :compiler {:output-to "resources/public/js/cljs_ajax_battleship_pre.js"
                           :optimizations :simple
                           :pretty-print false}}
               :prod
               {:source-paths ["src/cljs"]
                :compiler {:output-to "resources/public/js/cljs_ajax_battleship.js"
                           :optimizations :advanced
                           :pretty-print false}}}})
