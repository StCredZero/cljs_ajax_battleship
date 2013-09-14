(ns cljs_ajax_battleship.core
  (:use compojure.core)
  (:use clojure.set)
  (:use clojure.pprint)
  (:require [compojure.route :as route] 
            [compojure.handler :as handler]) 
  (:use ring.middleware.reload) 
  (:use ring.adapter.jetty))

;; site function creates a handler suitable for a standard website,
;; adding a bunch of standard ring middleware to app-route:
;; (def handler
;;   (handler/site bs-handler/app-routes))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Definitions

(def show-calc false)

(def rows "ABCDEFGHIJ")
(def cols (range 1 11))

(defn abs [x] (if (neg? x) (- x) x))
(defn rand-shipnum [] (+ (rand-int 100) (* 128 (rand-int 2))))

(defstruct player-state-struct
  :impacts
  :ships)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Definitions - Coordinates


(defn legal-coord? [c0]
  (and (string? c0)
       (contains? (set (seq rows)) (first c0))
       (contains? (set cols) (read-string (subs c0 1)))))

(defn legal-coords? [coords]
  (every? legal-coord? coords))

(defn coord [rnum cnum] 
  (let [c0 (str (char rnum) cnum)]
    (if (legal-coord? c0) c0 nil)))

(defn coord-row [c0] 
  {:pre [(legal-coord? c0)]} 
  (+ (- (int (first c0)) (int \A)) 1))

(defn coord-col [c0] 
  {:pre [(legal-coord? c0)]} 
  (read-string (subs c0 1)))

(defn coord->index [c0] 
  {:pre [(legal-coord? c0)]}
  (+ (* 10 (- (coord-row c0) 1)) (- (coord-col c0) 1))) 

(defn index->coord [idx]
  (coord (char (+ (int \A) (quot idx 10))) (+ (mod idx 10) 1)))

(defn coordsort-canonical [coords] 
  {:pre [(legal-coords? coords)]}
  (apply vector (sort-by coord->index coords)))

(defn coords-covertical? [coords]
  {:pre [(legal-coords? coords)]}
  (= 1 (count (set (map coord-col coords)))))

(defn coord-distance [c0 c1]
  {:pre [(legal-coord? c0)
         (legal-coord? c1)]}
  (+ (abs (- (coord-row c0) (coord-row c1)))
     (abs (- (coord-col c0) (coord-col c1)))))

(defn black-coord? [c0] 
  {:pre [(legal-coord? c0)]}
  (odd? (+ (coord-row c0) (coord-col c0))))

(def all-shots (for [row rows col cols] (coord row col)))
(def checkerboard (filter black-coord? all-shots))

(defn coord-change-by [offset-map c0] 
  (if (legal-coord? c0)
    (let [dx   (:dx offset-map)
          dy   (:dy offset-map)
          row1 (int (first c0))
          col1 (coord-col c0)]
      (coord (char (+ dy row1)) (+ dx col1)))
    nil))

(defn Y-combinator [f]
  ((fn [x] (x x))
   (fn [x]
     (f (fn [& args]
          (apply (x x) args))))))

(let [f (fn [x] (* x x))
      g (fn [n] (fn [x] [(n x) (n (n x))]))
      h (g f)]
      (h 2))

(def cardinal-foffsets
  [(defn offset-right  [c0] (coord-change-by {:dx 1 :dy 0}  c0))
   (defn offset-up     [c0] (coord-change-by {:dx 0 :dy -1} c0))
   (defn offset-left   [c0] (coord-change-by {:dx -1 :dy 0} c0))
   (defn offset-down   [c0] (coord-change-by {:dx 0 :dy 1}  c0))])

(def colinear-pair-foffsets 
  (zipmap cardinal-foffsets (map (fn [f] (comp f f)) cardinal-foffsets)))

(defn map-offset-funcs [offset-funcs c0]
  (let [offset-with (fn [f] (f c0))]
    (map offset-with offset-funcs)))

(defn map-offsets-filtered [offset-funcs c0]
  (filter legal-coord? (map-offset-funcs offset-funcs c0)))

(defn adjacent-to-coord  [c0] (map-offsets-filtered cardinal-foffsets c0))
(defn adjacent-to-coords [coords] 
  (remove (set coords) (flatten (map adjacent-to-coord coords))))

(defn adjacent-pair? [coord-pair] 
  {:pre [(sequential? coord-pair)
         (legal-coords? coord-pair)
         (= 2 (count coord-pair))]}
  (let [c0         (first coord-pair)
        equals-c0? (fn [cx] (= cx c0))]
    (not (nil? (some equals-c0? (map-offsets-filtered cardinal-foffsets (last coord-pair)))))))

(defn adjacent? [coords] 
  {:pre [(legal-coords? coords)]}
  (let [sort-lst (coordsort-canonical coords)
        pairs    (partition 2 1 sort-lst)]
    (every? adjacent-pair? pairs)))    

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Definitions - Game

(def ship-types [ "carrier" "battleship" "cruiser" "submarine" "destroyer" ])
(def ship-sizes (zipmap ship-types [5 4 3 3 2]))
(def start-ship-count (apply + (vals ship-sizes)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Initialization

(declare byte->ship)
(defn rand-ship [ship-type] 
  (let [ship (byte->ship (rand-shipnum) ship-type)]
    (if (= (count ship) (ship-sizes ship-type))
      ship
      (rand-ship ship-type))))
      
(defn rand-ships []
  (let [ships (apply conj (map rand-ship ship-types))]
    (if (= start-ship-count (count ships))
      ships
      (rand-ships))))

(defn create-player [impacts ship-map]
  (struct-map player-state-struct
    :impacts impacts
    :ships ship-map))

(defn new-players []
  [(create-player {} (rand-ships)) (create-player {} (rand-ships))])

(defn init-game []
  (do 
    (def player1 (atom (create-player {} (rand-ships))))
    (def player2 (atom (create-player {} (rand-ships))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Printing

(def ship-display-map
  {  "carrier"    "A"
     "battleship" "B"
     "cruiser"    "C"
     "submarine"  "S"
     "destroyer"  "D"
     "hit"        "X"
     "miss"       "-"})

(defn table-row-from-map [a-map row idxs f]      
  (let [ idx (first idxs)
        key (str row idx)
        val (f a-map key) ] 
    (if (empty? idxs)
      (identity {" " row})
      (conj {idx val} (table-row-from-map a-map row (rest idxs) f)))))

(defn table-rows-from-map [a-map rowcoll f]
  (if (empty? rowcoll)
    (identity [])
    (cons (table-row-from-map a-map (first rowcoll) cols f)
          (table-rows-from-map a-map (rest rowcoll) f))))

(defn ship-print-f [a-map key]
  (if (contains? a-map key)
    (let [ shipname (get a-map key) ]
      (ship-display-map shipname))
    (identity " ")))

(defn print-grid [f a-map ]
  (print-table (cons " " cols) (table-rows-from-map a-map rows f)))

(defn raw-print-side [player opponent]
  (let [player-map (merge (:ships player) (:impacts player))]
    (do
      (print-grid ship-print-f (:impacts opponent))
      (print-grid ship-print-f player-map))))

(defn print-side [player-atom opponent-atom]
  (let [player     (deref player-atom) 
        opponent   (deref opponent-atom)]
    (raw-print-side player opponent)))

(declare tactical-rank)
(defn print-tac [player]
  ship-print-f
  (print-grid ship-display-map (apply conj (cons {} (map (fn [x] [x (tactical-rank (deref player) x)]) all-shots)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementation

(defn swap-player! [player-atom impacts ships]
  (let [update-player 
        (fn [player-state impacts ships] (create-player impacts ships))]
  (swap! player-atom update-player impacts ships)))

(defn raw-shoot-at [player c0] 
  {:pre [(legal-coord? c0)]}
  (let [impacts     (:impacts player)
        ships       (:ships player)
        marker      (if (contains? ships c0)
                      (identity "hit")
                      (identity "miss"))
        new-impacts (conj impacts [c0 marker])]
    (create-player new-impacts ships))) 

(defn shoot-at [player-atom c0] 
  {:pre [(legal-coord? c0)]}
  (let [player     (deref player-atom)
        new-player (raw-shoot-at player c0)]
    (swap! identity (raw-shoot-at player c0)))) 

;     (swap-player! player-atom (conj impacts [c0 marker]) ships)

(defn coords-until [pred? offset-f c0]
  {:pre [(legal-coord? c0)]}
  (if (pred? c0) 
    '()
    (let [newc0 (offset-f c0)]
      (if (legal-coord? newc0)
        (cons c0 (coords-until pred? offset-f newc0))
        (cons c0 '())))))

(defn tactical-rank [player-data c0]
  {:pre [(legal-coord? c0)]}
  (let [impacts  (:impacts player-data) 
        pred?    (fn [x] (contains? #{"hit" "miss"} (impacts x)))
        dist-f   (fn [offset-f] (- (count (coords-until pred? offset-f c0)) 1))
        dist-map (zipmap [:right :up :left :down] (map dist-f cardinal-foffsets))]
    (+ 
     (* 10 (apply + (vals dist-map)))
     (- 10 (abs (- (:up dist-map) (:down dist-map))))
     (- 10 (abs (- (:left dist-map) (:right dist-map)))))))

(defn legal-shot? [player-data c0] 
  {:pre [(legal-coord? c0)]}
  (let [ impacts (:impacts player-data)]
    (not (contains? impacts c0))))

(defn ship-afloat [player-data ship]
  {:pre [(contains? (set ship-types) ship)]}
  (let [ships   (:ships   player-data)
        impacts (:impacts player-data)]
    (contains? (set (vals (merge ships impacts))) ship)))

(defn fleet-status [player-data]
  (for [a-type ship-types] [a-type (ship-afloat player-data a-type)]))

(defn rand-coord []
  (let [row    (char (+ (rand-int 10) (int \A)))
        column (+ 1 (rand-int 10))]
    (coord row column)))

(defn ship-at [player-data c0]
  {:pre [(legal-coord? c0)]}
  ((:ships player-data) c0))

(defn ship-afloat-at [player-data c0]
  {:pre [(legal-coord? c0)]}
  (let [ship (ship-at player-data c0)]
    (if (nil? ship)
      false
      (ship-afloat player-data ship))))

(defn ship-sunk-at [player-data c0]
  {:pre [(legal-coord? c0)]}
  (not (ship-afloat-at player-data c0)))

(defn filter-impacts [f player-data]
  (let [impacts (:impacts player-data)]
    (map first (filter f impacts))))

;true if coord is next to a pair of hits in impact-map
(defn next-to-pair-of-hits? [player-data c0]
  {:pre [(legal-coord? c0)]}
  (let [impact-map (:impacts player-data)
        is-hit?    (fn [c2]  (and (= "hit" (get impact-map c2))
                                  (ship-afloat-at player-data c2)))
        all-hits?  (fn [coords] (every? is-hit? coords))] 
    (not 
     (nil? 
      (some all-hits? 
            (for [offset-list colinear-pair-foffsets]
              (map-offset-funcs offset-list c0)))))))

;true if coord is a lone hit in impact-map
(defn hit-alone? [player-data c0]
  {:pre [(legal-coord? c0)]}
  (let [impact-map (:impacts player-data)
        no-hit?    (fn [c1] (or (not (= "hit" (get impact-map c1)))
                                (ship-sunk-at player-data c1)))]
    (and (ship-afloat-at player-data c0)
         (every? no-hit? (adjacent-to-coord c0)))))

(defn show-calc! [msg] (if show-calc (println msg)))

; Calculate the sensible next shots on behalf of player to take against opponent
(defn calc-next-shots [player opponent]
  (let [shots-taken  (:impacts opponent)]
    ;if no shots have been taken, just pick a random coordinate
    (if (empty? shots-taken) 
      (do (show-calc! "pick-random") (cons (rand-coord) #{}))
      ;find untargeted coords colinear and adjacent to a pair of hits
      (let [hits          (filter-impacts (fn [x] (= "hit" (last x))) opponent)
            untargeted?   (fn [c0] (not (contains? shots-taken c0)))
            next-to-hits  (filter untargeted? (adjacent-to-coords hits))
            adj-pair?     (fn [c0] (next-to-pair-of-hits? opponent c0))
            adj-pair-hits (filter adj-pair? next-to-hits)]
        (if (not (empty? adj-pair-hits))
          (do (show-calc! "adj-pair") (identity adj-pair-hits))
          ;find coords adjacent to untargeted hit coords that aren't next to another hit
          (let [single?     (fn [x] (hit-alone? opponent x))
                single-hits (filter single? hits)
                adj-singles (filter untargeted? (adjacent-to-coords single-hits))]
          (if (not (empty? adj-singles))
            (do (show-calc! "adj-single") (identity adj-singles))
            ;just output everything in the checkboard that hasn't been targeted yet
            (do (show-calc! "checkerboard") (filter untargeted? checkerboard)))))))))

(defn calc-next-shot [player opponent]
  (let [next-shots (seq (calc-next-shots player opponent))
        impacts    (:impacts opponent)
        keyfn      (fn [c0] (tactical-rank opponent c0))]
    ;tactical-rank has a nasty O(n^2) time complexity until the board fills up
    ;also random is a better strategy at first 
    (if (< 10 (count impacts)) 
      (rand-nth next-shots)
      (last (sort-by keyfn next-shots)))))
  

(defn take-turn []
  (do
    (let [p1-shot (calc-next-shot player1 player2)
          p2-shot (calc-next-shot player2 player1)]
    (println "player 1 shot" p1-shot)
    (shoot-at player2 p1-shot)
    (println "player 2 shot" p2-shot)
    (shoot-at player1 p2-shot)
    (print-side player1 player2)
    (print-side player2 player1))))

(defn play-turn [player1 player2 c0]
  (do (raw-shoot-at player2 c0)
      (calc-next-shot player2 player1))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Serialization 

(defn bigint->boolarray [x]
  (if (= x 0)
    [false]
    (if (= x 1)
      [true]
      (concat (bigint->boolarray (quot x 2)) [(odd? x)]))))

(defn boolarray->coords [boolarray]
  (loop [idx       0
         remaining boolarray
         result    #{}]
    (if (or (> idx 99) (empty? remaining))
      result
      (let [current (if (last remaining) #{(index->coord idx)} #{})]
        (recur (inc idx) (butlast remaining) (union result current))))))

(defn coords->bigint [coords]
  (loop [idx    99
         result (bigint 0)]
    (let [x (if (contains? coords (index->coord idx)) 1 0)]
      (if (not (>= idx 0))
        result
        (recur (dec idx) (+ x (* 2 result)))))))

(defn hex->int [s]
  (read-string (if (= (subs s 0 2) "0x") s (str "0x" s))))

(defn hexstring->coords [s]
    (boolarray->coords (bigint->boolarray (hex->int s))))
        
(defn coords->hexstring [coords]
  (format "%02x" (biginteger (coords->bigint coords))))    

(defn byte->ship [x ship-type]
  {:pre [(contains? ship-sizes ship-type)
         (>= x 0)]}
  (let [direction (if (>= x 128) offset-down offset-right)
        idx       (if (= direction offset-down) (- x 128) x)
        c0        (index->coord (mod idx 100))
        ship-len  (ship-sizes ship-type)
        pred?     (fn [c1] (>= (coord-distance c0 c1) ship-len))
        coords    (coords-until pred? direction c0)]
    (zipmap coords (repeat ship-type))))

(defn ship->byte [ship-map ship-type]
  (let [has-ship-type? (fn [k] (= ship-type (ship-map k)))
        coords         (filter has-ship-type? (keys ship-map))
        raw-idx        (coord->index (first (sort-by coord->index coords)))]
    (if (coords-covertical? coords) (+ 128 raw-idx) raw-idx)))

(defn ship->string [ship-map ship-type]
  (format "%02x" (ship->byte ship-map ship-type)))

(defn player->string [player-data]
  (let [ships      (:ships player-data)
        impacts    (:impacts player-data)
        f          (fn [ship-type] (ship->string ships ship-type))
        ships-str  (apply str (map f ship-types))
        coords-str (coords->hexstring (set (keys impacts)))]
    (str ships-str coords-str)))

(defn string->ships [player-str]
  (let [format  (partition 2 1 (range 0 12 2))
        f       (fn [idxs] (hex->int (apply subs (cons player-str idxs))))
        n->ship (fn [x] (byte->ship (first x) (last x)))]
    (apply conj (map n->ship (zipmap (map f format) ship-types)))))

(defn string->impacts [ships player-str]
  (let [impactf (fn [c0] {c0 (if (contains? ships c0) "hit" "miss")})
        imp-str (subs player-str 10)
        coords  (map impactf (hexstring->coords imp-str))]
    (if (empty? coords)
      {}
      (apply conj (cons {} coords)))))

(defn string->player [player-str]
  (let [ships   (string->ships player-str)
        impacts (string->impacts ships player-str)]
    (create-player impacts ships)))

(defn valid-player-string? [player-str]
  (let [hexchr? (fn [c] (contains? (set "0123456789abcdefABCDEF") c))]
    (and (string? player-str)
         (>= (count player-str) 12)
         (every? hexchr? (seq player-str)))))

(defn valid-play? [player1-str player2-str shot-str]
  (let [canonical-shot (try (clojure.string/upper-case shot-str) (catch Exception e1 nil))]
      (and (valid-player-string? player1-str)
           (valid-player-string? player2-str)
           (legal-coord? canonical-shot))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(init-game)



