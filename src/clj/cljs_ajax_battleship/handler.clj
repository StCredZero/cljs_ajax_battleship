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

(defn create-ship-index-f [player]
  (fn [c0]
    (let [ship-at (fn [c1] (get (:ships player) c1))]
      (if (nil? (ship-at c0))
        0
        (let [not-same-ship-type? (fn [c1] (not (= (ship-at c0) (ship-at c1))))
              count-up   (count (bship/coords-until not-same-ship-type? bship/offset-up c0))
              count-down (count (bship/coords-until not-same-ship-type? bship/offset-down c0))
              count-left (count (bship/coords-until not-same-ship-type? bship/offset-left c0))
              direction  (if (> (+ count-down count-up) 2) "v" "h")]
          (str direction (max count-up count-left)))))))

(defn battleship-table-cell [id-prefix render-f coord]
  (str "<td id=\"" id-prefix coord "\">" (render-f coord) "</td>"))

(defn battleship-table-row [id-prefix render-f row]
  (str "<tr>"
       "<td class=\"rounded-rowlabel\">" row "</td>"
       (apply str
              (for [col bship/cols]
                (let [coord (str row col)]
                  (battleship-table-cell id-prefix render-f coord))))
       "</tr>"))

(defn battleship-table-header []
  (str
   "<thead><tr>"
   "<th scope=\"col\" class=\"rounded-ul-corner\"> </th>"
   (apply str
          (for [col bship/cols]
            (str "<th scope=\"col\" class=\"rounded-col" col "\">" col "</th>")))
   "</tr></thead>"))

(defn battleship-table [table-display-name id-prefix render-f]
  (str "<table id=\""id-prefix "\" summary=\"Battleship\">"
       (battleship-table-header)
       "<tfoot>
        <tr>"
        "<td colspan=\"10\" class=\"rounded-foot-left\"><em>" table-display-name "</em></td>"
        "<td class=\"rounded-foot-right\">&nbsp;</td>
        </tr>
        </tfoot>"
        "<tbody>"
       (apply str (for [row bship/rows]
                    (battleship-table-row id-prefix render-f row)))
       "</tbody>
        </table>"
       ))

(defn relative-get-url [player1 player2]
  (let [player-string1 (bship/player->string player1)
        player-string2 (bship/player->string player2)]
    (str "/ajbs?p1=" player-string1 "&p2=" player-string2 "&s=")))

(defn empty-display-f [coord]
  (str "<img src=\"/table-images/bs-back5.png\">"))

(defn create-hit-display-f [player1 player2]
  (fn [coord]
    (let [player-map   (merge (:ships player1) (:impacts player1))
          coord-status (get player-map coord)
          ship-index   (create-ship-index-f player1)]
      (if (nil? coord-status)
        (empty-display-f coord)
        (if (= coord-status "hit")
          (str "<img src=\"/table-images/explosion2.png\">")
          (if (= coord-status "miss")
            (str "<img src=\"/table-images/miss2.png\">")
            (let [disp-letter (get bship/ship-display-map coord-status)]
              (if (nil? disp-letter)
                "ERR"
                (str "<img src=\"/table-images/" disp-letter (ship-index coord) ".png\">")))))))))

(defn create-shot-display-f [player1 player2]
  (fn [coord]
    (let [relative-url (relative-get-url player1 player2)
          coord-status (get (:impacts player2) coord)]
      (if (nil? coord-status)
        (str "<a href=\"" relative-url coord "\"><img src=\"/table-images/bs-back5.png\"></a>")
        (if (= coord-status "hit")
          (str "<img src=\"/table-images/explosion2.png\">")
          (if (= coord-status "miss")
            (str "<img src=\"/table-images/miss2.png\">")
            ("ERR")))))))

(defn ajbs-html [player1 player2]
  (let [shot-display-f (create-shot-display-f player1 player2)
        hit-display-f  (create-hit-display-f player1 player2)]
    (str
     "<!DOCTYPE html>"
     "<html class=\"bs-html\" lang=\"en-US\">"
     "<head>"
     "<link rel=\"stylesheet\" type=\"text/css\" href=\"/css/styles.css\" />"
     "<script src=\"http://code.jquery.com/jquery-1.9.1.js\"></script>"
     "<script src=\"http://code.jquery.com/ui/1.10.3/jquery-ui.js\"></script>"
     "</head>"
     "<body bgcolor=#ffffff>"
     (battleship-table "Targeting" "targeting" shot-display-f)
     (battleship-table "Fleet Status" "status" hit-display-f)
     "<script src=\"/js/cljs_ajax_battleship_dbg.js\"></script>"
     "<script>"
     "$( document ).ready(function() {"
     "console.log( \"document loaded\" ); "
     "});"
     "$(window).load(function(){"
     "cljs_ajax_battleship.fleet.init();"
     "console.log( \"initialized\" ); "
     "});"
     "</script>"
     "</body>
      </html>")))

(defn deploy-html [player1 player2]
  (let [shot-display-f (create-shot-display-f player1 player2)
        hit-display-f  (create-hit-display-f player1 player2)]
    (str
     "<!DOCTYPE html>"
     "<html class=\"bs-html\" lang=\"en-US\">"
     "<head>"
     "<link rel=\"stylesheet\" type=\"text/css\" href=\"/css/styles.css\" />"
     "<script src=\"http://code.jquery.com/jquery-1.9.1.js\"></script>"
     "<script src=\"http://code.jquery.com/ui/1.10.3/jquery-ui.js\"></script>"
     "</head>"
     "<body bgcolor=#ffffff>"
     (battleship-table "Fleet Deployment" "deploy" empty-display-f)
     "<img id=\"harbor-image\" src=\"/table-images/harbor1.png\" >"
     "<div id=\"harbor\" >"
     "<div id=\"carrier1\"    class=\"dragthing\"><img id=\"carrier1i\"    src=\"/table-images/Ah.png\"></div>"
     "<div id=\"battleship1\" class=\"dragthing\"><img id=\"battleship1i\" src=\"/table-images/Bh.png\"></div>"
     "<div id=\"cruiser1\"    class=\"dragthing\"><img id=\"cruiser1i\"    src=\"/table-images/Ch.png\"></div>"
     "<div id=\"submarine1\"  class=\"dragthing\"><img id=\"submarine1i\"  src=\"/table-images/Sh.png\"></div>"
     "<div id=\"destroyer1\"  class=\"dragthing\"><img id=\"destroyer1i\"  src=\"/table-images/Dh.png\"></div>"
     "<div id=\"carrier0\"    class=\"dragthing\"><img id=\"carrier0i\"    src=\"/table-images/Av.png\"></div>"
     "<div id=\"battleship0\" class=\"dragthing\"><img id=\"battleship0i\" src=\"/table-images/Bv.png\"></div>"
     "<div id=\"cruiser0\"    class=\"dragthing\"><img id=\"cruiser0i\"    src=\"/table-images/Cv.png\"></div>"
     "<div id=\"submarine0\"  class=\"dragthing\"><img id=\"submarine0i\"  src=\"/table-images/Sv.png\"></div>"
     "<div id=\"destroyer0\"  class=\"dragthing\"><img id=\"destroyer0i\"  src=\"/table-images/Dv.png\"></div>"
     "</div>"
     "<form id='startgame' action='/startgame' method='GET'>"
         "<input id='shipdata' type='hidden' name='p1' value=''>"
         "<input type='submit' value='Start Game'>"
     "</form>"
     "<script src=\"/js/cljs_ajax_battleship_dbg.js\"></script>"
     "<script>"
     ;; "cljs_ajax_battleship.fleet.init();"
     ;; "$( document ).ready(function() {"
     ;; "console.log( \"document loaded\" ); "
     ;; "});"
     "$(window).load(function(){"
     "cljs_ajax_battleship.fleet.init();"
     "console.log( \"initialized\" ); "
     "});"
     "</script>"
     "</body>
      </html>")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Routes

(defroutes my-routes
  (GET "/" []
       (let [players (bship/new-players)]
         (deploy-html (first players) (last players))))

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

  (GET "/startgame*"  {params :query-params} (str params))
  
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
