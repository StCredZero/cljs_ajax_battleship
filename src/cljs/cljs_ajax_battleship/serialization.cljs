(ns cljs_ajax_battleship.serialization
  (:require [clojure.set :as sets]))
;; (in-ns 'cljs_ajax_battleship.serialization)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Definitions

(def rows "ABCDEFGHIJ")
(def cols (range 1 11))

(defn abs [x] (if (neg? x) (- x) x))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Definitions - Coordinates


(defn legal-coord? [c0]
  (and (string? c0)
       (contains? (set (seq rows)) (first c0))
       (contains? (set cols) (js/parseInt (subs c0 1)))))

(defn legal-coords? [coords]
  (every? legal-coord? coords))

(defn coord [rnum cnum] 
  (let [c0 (str (char rnum) cnum)]
    (if (legal-coord? c0) c0 nil)))

(defn coord-row [c0] 
  {:pre [(legal-coord? c0)]} 
  (+ (- (.charCodeAt (first c0) 0) (.charCodeAt \A 0)) 1))

(defn coord-col [c0] 
  {:pre [(legal-coord? c0)]} 
  (js/parseInt (subs c0 1)))

(defn coord->index [c0] 
  {:pre [(legal-coord? c0)]}
  (+ (* 10 (- (coord-row c0) 1)) (- (coord-col c0) 1))) 

(defn index->coord [idx]
  (coord (char (+ (.charCodeAt \A 0) (quot idx 10))) (+ (mod idx 10) 1)))

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
          row1 (.charCodeAt (first c0) 0)
          col1 (coord-col c0)]
      (coord (char (+ dy row1)) (+ dx col1)))
    nil))

(defn offset-right  [c0] (coord-change-by {:dx 1 :dy 0}  c0))

(defn offset-up     [c0] (coord-change-by {:dx 0 :dy -1} c0))

(defn offset-left   [c0] (coord-change-by {:dx -1 :dy 0} c0))

(defn offset-down   [c0] (coord-change-by {:dx 0 :dy 1}  c0))

(def cardinal-foffsets
  [offset-right offset-up offset-left offset-down])

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
;; Serialization 


(defn boolarray->coords
  ;; ^{:doc "Takes all-coordinates representing all
  ;; possible coordinates on a battleship map, and an array of boolean
  ;; values and converts them into a set off coordinates. The array (like
  ;; a little-endian bit array) represents a battleship map where a true
  ;; value at an index indicates a presence, false indicates an absence."
  ;;   :pre [(sequential? present-coordinates)
  ;;         (every? (fn [item]
  ;;                   (or (= false item)
  ;;                       (= true item))) present-coordinates)]
  ;;   :post [(set? %)]}
  [all-coordinates present-coordinates]

  ; Mapping and filtering out nils
  (->> (map (fn [coordinate present?]
             (when present? coordinate)) all-coordinates present-coordinates)
      (filter identity)
      set))

(defn hex->int [string]
  (js/parseInt string 16))

(def hex->boolarray-map
  {"0" [false false false false]
   "1" [true  false false false]
   "2" [false true  false false]
   "3" [true  true  false false]
   "4" [false false true  false]
   "5" [true  false true  false]
   "6" [false true  true  false]
   "7" [true  true  true  false]
   "8" [false false false true ]
   "9" [true  false false true ]
   "a" [false true  false true ]
   "b" [true  true  false true ]
   "c" [false false true  true ]
   "d" [true  false true  true ]
   "e" [false true  true  true ]
   "f" [true  true  true  true ]
   })

(def boolarray->hex-map
  { [false false false false] "0"
    [true  false false false] "1"
    [false true  false false] "2"
    [true  true  false false] "3"
    [false false true  false] "4"
    [true  false true  false] "5"
    [false true  true  false] "6"
    [true  true  true  false] "7"
    [false false false true ] "8"
    [true  false false true ] "9"
    [false true  false true ] "a"
    [true  true  false true ] "b"
    [false false true  true ] "c"
    [true  false true  true ] "d"
    [false true  true  true ] "e"
    [true  true  true  true ] "f"
   })

(defn hex->boolarray [hex-str]
  (if (= 1 (count hex-str))
    (get hex->boolarray-map hex-str)
    (let [strlen            (count hex-str)
          last-idx          (- strlen 1)
          least-significant (subs hex-str last-idx strlen)
          the-rest          (subs hex-str 0 last-idx)]
      (concat (hex->boolarray least-significant) (hex->boolarray the-rest)))))

(defn boolarray->hex [boolarray]
  ;; normalized-len4 returns a vector of 4 elements, pads false on
  ;;right
  {:pre [ (vector? boolarray)]}
  (let [normalized-len4
        (fn [v] (subvec (apply vector (concat v [false false false false]))0 4))]
    (if (= (count boolarray) 0)
      ""
      (if (<= (count boolarray) 4)
        (get boolarray->hex-map (normalized-len4 boolarray))
        (str (boolarray->hex (subvec boolarray 4))
             (boolarray->hex (subvec boolarray 0 4)))))))

(defn boolarray->impacts [ship-map boolarray]
  (let [impact-coords (map first (filter last (zipmap all-shots boolarray)))
        hit-or-miss   (fn [c0] (if (contains? ship-map c0) {c0 "hit"} {c0 "miss"}))]
    (apply merge (cons {} (map hit-or-miss impact-coords)))))

(defn impacts->boolarray [impacts-map]
  (map (fn [c0] (contains? impacts-map c0)) all-shots))

(defn hexstring->coords [s]
  (let [boolarray (hex->boolarray s)
        _         (println boolarray)
        _  (println (sequential? boolarray))]
    (boolarray->coords all-shots boolarray)))
        
(defn coords->hexstring [coords]
  (let [boolarray (mapv (fn [c0] (contains? coords c0)) all-shots)]
    (boolarray->hex boolarray)))    

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

;; (defn ship->byte [ship-map ship-type]
;;   (let [has-ship-type? (fn [k] (= ship-type (ship-map k)))
;;         coords         (filter has-ship-type? (keys ship-map))
;;         raw-idx        (coord->index (first (sort-by coord->index coords)))]
;;     (if (coords-covertical? coords) (+ 128 raw-idx) raw-idx)))

;; (defn ship->string [ship-map ship-type]
;;   (format "%02x" (ship->byte ship-map ship-type)))

;; (defn player->string [player-data]
;;   (let [ships      (:ships player-data)
;;         impacts    (:impacts player-data)
;;         f          (fn [ship-type] (ship->string ships ship-type))
;;         ships-str  (apply str (map f ship-types))
;;         coords-str (coords->hexstring (set (keys impacts)))]
;;     (str ships-str coords-str)))

;; (defn string->ships [player-str]
;;   (let [format  (partition 2 1 (range 0 12 2))
;;         f       (fn [idxs] (hex->int (apply subs (cons player-str idxs))))
;;         n->ship (fn [x] (byte->ship (first x) (last x)))]
;;     (apply conj (map n->ship (zipmap (map f format) ship-types)))))

;; (defn string->impacts [ships player-str]
;;   (let [impactf (fn [c0] {c0 (if (contains? ships c0) "hit" "miss")})
;;         imp-str (subs player-str 10)
;;         coords  (map impactf (hexstring->coords imp-str))]
;;     (if (empty? coords)
;;       {}
;;       (apply conj (cons {} coords)))))

;; (defn string->impacts2 [ships player-str]
;;   (let [impacts-hexstr    (subs player-str 10)
;;         impacts-boolarray (hex->boolarray impacts-hexstr)]
;;     (boolarray->impacts ships impacts-boolarray)))

;; (defn string->player [player-str]
;;   (let [ships   (string->ships player-str)
;;         impacts (string->impacts ships player-str)]
;;     (create-player impacts ships)))

;; (defn string->player2 [player-str]
;;   (let [ships   (string->ships player-str)
;;         impacts (string->impacts2 ships player-str)]
;;     (create-player impacts ships)))

;; (defn valid-player-string? [player-str]
;;   (let [hexchr? (fn [c] (contains? (set "0123456789abcdefABCDEF") c))]
;;     (and (string? player-str)
;;          (>= (count player-str) 12)
;;          (every? hexchr? (seq player-str)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
