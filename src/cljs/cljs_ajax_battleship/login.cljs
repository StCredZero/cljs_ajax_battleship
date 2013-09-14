(ns cljs_ajax_battleship.login
  (:require-macros [hiccups.core :as h])
  (:require [domina :as dom]
            [hiccups.runtime :as hiccupsrt]
            [domina.events :as ev]))

;; define the function to be attached to form submission event
(defn validate-form []
  ;; get email and password element from their ids in the HTML form
  (let [email    (dom/by-id "email")
        password (dom/by-id "password")]
    (if (and (> (count (dom/value email)) 0)
             (> (count (dom/value password)) 0))
      true
      (do (js/alert "Please, complete the form!")
          false))))

;; define the function to attach validate-form to onsubmit event of
;; the form
(defn ^:export init []
  ;; verify that js/document exists and that it has a getElementById
  ;; property
  (when (and js/document
             (aget js/document "getElementById"))
    ;; get loginForm by element id and set its onsubmit property to
    ;; our validate-form function
    (ev/listen! (dom/by-id "loginButton") :click validate-form)

    ;; (let [login-form (.getElementById js/document "loginForm")]
    ;;   (set! (.-onsubmit login-form) validate-form)))
  ))

;; initialize the HTML page in unobtrusive way
;; (set! (.-onload js/window) init)

