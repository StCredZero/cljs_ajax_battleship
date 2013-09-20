(ns cljs_ajax_battleship.board
  (:require [cljs_ajax_battleship.serialization :as serialization]))

(defn rand-ship-index []
  (+ (rand-int 100) (* 128 (rand-int 2))))

(defn rand-ship [ship-type]
  (let [ship (serialization/byte->ship (rand-ship-index) ship-type)]
    (if (= (count ship) (serialization/ship-sizes ship-type))
      ship
      (rand-ship ship-type))))

(defn rand-ships []
  (let [ships (apply conj (map rand-ship serialization/ship-types))]
    (if (= serialization/start-ship-count (count ships))
      ships
      (rand-ships))))

(defn create-player [impacts ship-map]
  {:impacts impacts
   :ships ship-map})

(defn new-players []
  [(create-player {} (rand-ships))
   (create-player {} (rand-ships))])
