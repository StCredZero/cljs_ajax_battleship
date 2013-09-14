(ns cljs_ajax_battleship.handler
  (:use [compojure.core]
        [ring.adapter.jetty]
        [ring.middleware.reload]
        [ring.middleware.stacktrace])
  (:require [compojure.handler :refer [site]]
            [compojure.route :as route]
            [cheshire.core :as json]
            [cljs_ajax_battleship.core :as bship]
            [shoreleave.middleware.rpc :refer [defremote wrap-rpc]]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; HTML Generation

(defn battleship-html [player1 player2]
  (let [player-string1 (bship/player->string player1)
        player-string2 (bship/player->string player2)]
    (str "<pre>"
         (with-out-str (bship/raw-print-side player1 player2))
         "</pre>"
         "<form action='/bs' method='get'>"
         "<input type='hidden' name='p1' value='" player-string1 "'>"
         "<input type='hidden' name='p2' value='" player-string2 "'>"
         "<input type='text' name='s'>"
         "<input type='submit' value='Submit'>"
         "</form>")))

(defn battleship-table-cell [grid-map render-map relative-url coord]
  (str "<td><a href=\"" relative-url coord "\">" coord "</a></td>"))

(defn battleship-table-row [grid-map render-map relative-url row]
  (str "<tr>"
       "<td class=\"rounded-rowlabel\">" row "</td>"
       (apply str
              (for [col bship/cols]
                (let [coord (str row col)]
                  (battleship-table-cell grid-map render-map relative-url coord))))
       "</tr>"))

(defn battleship-table-header []
  (apply str
         (for [col bship/cols]
           (str "<th scope=\"col\" class=\"rounded-col" col "\">" col "</th>"))))

(defn battleship-table [grid-map render-map relative-url]
  (str "<table id=\"rounded-corner\" summary=\"Battleship\">
       <thead>
       <tr>
       <th scope=\"col\" class=\"rounded-ul-corner\"> </th>"
       (battleship-table-header)
       "</thead>
        <tfoot>
        <tr>
        <td colspan=\"10\" class=\"rounded-foot-left\">
        <em>The above data were fictional and made up, please do not sue me</em></td>
        <td class=\"rounded-foot-right\">&nbsp;</td>
        </tr>
        </tfoot>
        <tbody>"
       (apply str (for [row bship/rows]
                    (battleship-table-row grid-map render-map relative-url row)))
       "</tbody>
        </table>"))

(defn ajbs-html [player1 player2]
  (let [player-string1 (bship/player->string player1)
        player-string2 (bship/player->string player2)
        relative-url   (str "/ajbs?p1=" player-string1 "&p2=" player-string2 "&s=")]
    (str
     "<!DOCTYPE html>
      <html class=\"bs-html\" lang=\"en-US\">
      <head>
      <link rel=\"stylesheet\" type=\"text/css\" href=\"/css/styles.css\" />
      </head>
      <body>"
     (battleship-table {} {} relative-url)
     "</body>
      </html>")))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Routes

(defroutes my-routes
  (GET "/" []
       (let [players (bship/new-players)]
         (battleship-html (first players) (last players))))

  ;; serve static pages saved in resources/public directory
  (route/resources "/")
  
  (GET "/bs*" {params :query-params}
       (let [player-string1 (get params "p1")
             player-string2 (get params "p2")
             shot (clojure.string/upper-case (get params "s"))]
         (if (not (bship/legal-coord? shot))
           (str "<pre> '" shot "' is not a legal shot!</pre>")
           (if (not (bship/valid-player-string? player-string1))
             (str "<pre> '" player-string1 "' is not a legal player 1!</pre>")
             (if (not (bship/valid-player-string? player-string2))
               (str "<pre> '" player-string2 "' is not a legal player 2!</pre>")
               ( let [player1        (bship/string->player player-string1)
                      player2        (bship/string->player player-string2)
                      canonical-shot (clojure.string/upper-case shot)
                      calced-shot    (bship/calc-next-shot player2 player1)
                      new-player1    (bship/raw-shoot-at player1 calced-shot)
                      new-player2    (bship/raw-shoot-at player2 canonical-shot)]
                (battleship-html new-player1 new-player2)))))))

  (GET "/ajbs*" {params :query-params}
       (let [player-string1 (get params "p1")
             player-string2 (get params "p2")
             shot (clojure.string/upper-case (get params "s"))]
         (if (not (bship/legal-coord? shot))
           (str "<pre> '" shot "' is not a legal shot!</pre>")
           (if (not (bship/valid-player-string? player-string1))
             (str "<pre> '" player-string1 "' is not a legal player 1!</pre>")
             (if (not (bship/valid-player-string? player-string2))
               (str "<pre> '" player-string2 "' is not a legal player 2!</pre>")
               ( let [player1        (bship/string->player player-string1)
                      player2        (bship/string->player player-string2)
                      canonical-shot (clojure.string/upper-case shot)
                      calced-shot    (bship/calc-next-shot player2 player1)
                      new-player1    (bship/raw-shoot-at player1 calced-shot)
                      new-player2    (bship/raw-shoot-at player2 canonical-shot)]
                (ajbs-html new-player1 new-player2)))))))
  
  (GET "/hi*" {params :query-params}
       (str params))
  
  (POST "/posttest/:val" [val] (str "post test val was " val))
  
  (route/not-found "Page not found"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Remotes

(defremote calculate [quantity price tax discount]
  (-> (* quantity price)
      (* (+ 1 (/ tax 100)))
      (- discount)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; site function creates a handler suitable for a standard website,
;; adding a bunch of standard ring middleware to app-route:
(def handler
  (compojure.handler/site my-routes))

(def app
  (-> (var handler)          ;;(var my-routes)
      compojure.handler/api
      (wrap-rpc)
      (wrap-stacktrace)
      (site)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utility functions

(defn start [port background?]
  (println "Starting on port " port (when background? "in the background") "with " app)
  (defonce server
    (run-jetty
     (var app)
     {:port port
      :join? (not background?)})))

(defn stop [] (.stop server))
