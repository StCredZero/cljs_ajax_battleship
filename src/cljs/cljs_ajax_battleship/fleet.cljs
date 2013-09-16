(ns cljs_ajax_battleship.fleet                  ;; (in-ns 'cljs_ajax_battleship.fleet)
  (:require-macros [hiccups.core :as h])
  (:require [jayq.core :as jq]
            [domina :as dom]
            [hiccups.runtime :as hiccupsrt]
            [domina.events :as ev]
            [shoreleave.remotes.http-rpc :refer [remote-callback]]
            [cljs.reader :refer [read-string]])
  (:use [jayq.core :only [$]]))

(def original-pos (atom {}))
(def last-pos (atom {}))
(def ship-map (atom {}))

(def ship-types [ "carrier" "battleship" "cruiser" "submarine" "destroyer" ])
(def ship-sizes (zipmap ship-types [5 4 3 3 2]))

(defn add-help []
  (dom/append! (dom/by-id "cruiser-h")
               (h/html [:div.help "Drag to place"])))

(defn remove-help []
  (dom/destroy! (dom/by-class "help")))

(defn deploy-td-coll []
  (filter (fn [x] (> (count (.-id x)) 0))
          (js->clj ($ "#deploy td"))))

(defn contains-pt? [elem {x :x y :y}]
  (let [jqo      ($ elem)
        position (.position jqo)
        top      (.-top position)
        left     (.-left position)
        height   (.height jqo)
        width    (.width jqo)
        bottom   (+ top height)
        right    (+ left width)]
    (and (>= x left)
         (<= x right)
         (>= y top)
         (<= y bottom))))

(defn coord-from-id [id-str]
  (let [coord-chars    (set (map str (seq "ABCDEFGHIJ0123456789")))
        is-coord-char? (fn [c] (contains? coord-chars c))]
    (apply str (filter is-coord-char? id-str))))

(defn coord-of-elem [elem]
  (coord-from-id (.-id elem)))

(defn midpoint [elem]
  (let [jqo      ($ elem)
        position (.position jqo)
        top      (.-top position)
        left     (.-left position)
        height   (.height jqo)
        width    (.width jqo)]
    {:x (+ left (/ width 2))
     :y (+ top (/ height 2))}))

(defn contains-midpoint? [elem1 elem2]
  (contains-pt? elem1 (midpoint elem2)))

(defn covered-coords [ship]
  (let [ship-elem      ($ ship)
        covers-target? (fn [elem] (contains-midpoint? ship-elem elem))
        covered        (filter covers-target? (deploy-td-coll))
        covered-ids    (map (fn [x] (coord-of-elem x)) covered)]
    (map (fn [x] (coord-of-elem x)) covered)))

(def number-set (set (seq "0123456789")))

(defn ship-type [id-str]
  (let [not-number? (fn [c] (not (contains? number-set c)))]
    (apply str (filter not-number? id-str))))

(defn counterpart-id [id-str]
  (let [suffix      (apply str (filter number-set (seq id-str)))
        inverted    (mod (+ 1 (js/parseInt suffix)) 2)]
    (str (ship-type id-str) inverted)))

(defn counterpart-ship [ship]
  (let [ship-id (.-id ship)]
    (counterpart-id ship-id)))

(defn return-to-origin [ship-id]
  (let [origin (get (deref original-pos) ship-id)]
     (.animate ($ (str "#" ship-id)) (apply js-obj origin))))

(defn return-to-last-pos [ship-id]
  (let [origin (get (deref last-pos) ship-id)]
     (.animate ($ (str "#" ship-id)) (apply js-obj origin))))

(defn get-current-ship-positions []
  (apply merge
         (for [dragthing ($ ".dragthing")]
           (let [position  (.position ($ dragthing))
                 x         (.-left position)
                 y         (.-top position)
                 point     ["left" x "top" y]]
             {(.-id dragthing) point}))))

(defn calc-ship-map []
  (apply merge
         (flatten 
          (for [dthing ($ ".dragthing")]
            (let [coords   (covered-coords dthing)
                  id-str   (.attr ($ dthing) "id")]
              (map (fn [c0] {c0 (ship-type id-str)}) coords))))))

(defn write-ship-map [ship-map]
  (.val ($ "#shipdata") (str ship-map)))

(defn start-ship-drag [event ui]
  (let [mytarget (.-target event)
        my-id    (.-id mytarget)]
    (do
      (return-to-origin (counterpart-id my-id)))))

(defn drag-ship [event ui] nil)

(defn stop-ship-drag [event ui]
  (let [mytarget        (.-target event)
        my-id           (.-id mytarget)
        coords          (covered-coords mytarget)
        is-empty-coord? (fn [c0] (not (contains? (deref ship-map) c0)))]
    (do
      (if (not (every? is-empty-coord? coords))
        (return-to-last-pos my-id)
        (do (swap! last-pos get-current-ship-positions) 
            (swap! ship-map calc-ship-map)
            (write-ship-map (deref ship-map)))))))

(defn init-ship-marker! [ship]
  (.draggable ship
              (js-obj "grid"  (array 28 28)
                      "start" start-ship-drag
                      "stop"  stop-ship-drag)))

(defn ^:export init []
  (do
    (swap! original-pos get-current-ship-positions)
    (doall (for [dthing ($ ".dragthing")]
             (init-ship-marker! ($ dthing))))
    (js/console.log "cljs initialized")))

;; (in-ns 'cljs_ajax_battleship.fleet)

