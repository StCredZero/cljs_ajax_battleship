(ns cljs_ajax_battleship.fleet
  (:require-macros [hiccups.core :as h])
  (:require [jayq.core :as jq]
            [domina :as dom]
            [hiccups.runtime :as hiccupsrt]
            [domina.events :as ev]
            [shoreleave.remotes.http-rpc :refer [remote-callback]]
            [cljs.reader :refer [read-string]])
  (:use [jayq.core :only [$]]))

(defn add-help []
  (dom/append! (dom/by-id "draggable4")
               (h/html [:div.help "Drag to place"])))

(defn remove-help []
  (dom/destroy! (dom/by-class "help")))

(defn what-is []
  (.draggable ($ "#draggable4") { "grid" [20 20]}))

(defn ^:export init []
  (when (and js/document
             (aget js/document "getElementById"))
    (jq/bind ($ "#draggable4") :mouseup (fn [] (js/alert "Hi!")))

    ;; $( "#draggable4" ).draggable({ grid: [ 28,28 ] });
    (.draggable ($ "#draggable1") (js* "{grid: [28, 28]}"))
    (.draggable ($ "#draggable2") (js* "{grid: [28, 28]}"))
    (.draggable ($ "#draggable3") (js* "{grid: [28, 28]}"))
    (.draggable ($ "#draggable4") (js* "{grid: [28, 28]}"))
    (.draggable ($ "#draggable5") (js* "{grid: [28, 28]}"))
    
    (ev/listen! (dom/by-id "draggable4") :mouseover add-help)
    (ev/listen! (dom/by-id "draggable4") :mouseout remove-help)))

;; (.-top (.position ($ "#draggable4")))
