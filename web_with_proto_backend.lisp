;;; Copyright (C) 2014 Adam Tornhill
;;;
;;; Distributed under the GNU General Public License v3.0,
;;; see http://www.gnu.org/licenses/gpl.html

;;; The following module contains parts of the source code for
;;; my book Lisp for the Web. You can get a copy of the book here:
;;; https://leanpub.com/lispweb

(defpackage :retro-games
  (:use :cl :cl-who :hunchentoot :parenscript))

(in-package :retro-games)

;; Domain model
;; ============

;; A simple domain model of our games together with
;; access methods.

(defclass game ()
  ((name  :reader   name 
          :initarg  :name)
   (votes :accessor votes 
          :initform 0)))

;; By default the printed representation of CLOS objects isn't
;; particularly informative to a human. We can override the default
;; behaviour by specializing the generic function
;; print-object for our game (print-unreadable-object is
;; just a standard macro that helps us with the stream set-up
;; and display of type information).
(defmethod print-object ((object game) stream)
  (print-unreadable-object (object stream :type t)
    (with-slots (name votes) object
      (format stream "name: ~s with ~d votes" name votes))))

(defmethod vote-for (game)
  (incf (votes game)))

;; Backend
;; =======

;; A prototypic backend that stores all games in
;; a list. Later, we'll move to a persistent storage and
;; only need to modify these accessor functions:

(defvar *games* '())

;; We encapsulate all knowledge of the concrete storage
;; medium in the following functions:

(defun game-from-name (name)
  (find name *games* :test #'string-equal :key  #'name))

(defun game-stored? (game-name)
  (game-from-name game-name))

(defun games ()
  (sort (copy-list *games*) #'> :key #'votes))

(defun add-game (name)
  (unless (game-stored? name)
    (push (make-instance 'game :name name) *games*)))

;; Web Server - Hunchentoot

(defun start-server (port)
  (start (make-instance 'easy-acceptor :port port)))

(defun publish-static-content ()
  (push (create-static-file-dispatcher-and-handler
         "/logo.jpg" "static/Commodore64.jpg") *dispatch-table*)
  (push (create-static-file-dispatcher-and-handler
         "/retro.css" "static/retro.css") *dispatch-table*))

;; DSL for our web pages
;; =====================

;; Here we grow a small domain-specific language for
;; creating dynamic web pages.

; Control the cl-who output format (default is XHTML, we 
; want HTML5):
(setf (html-mode) :html5)

;; This is the initial version of our standard page template.
;; We'll evolve it later in the tutorial, making it accept
;; scripts as well (used to inject JavaScript for validation
;; into the HTML header).
(defmacro standard-page-1 ((&key title) &body body)
  "All pages on the Retro Games site will use the following macro;
   less to type and a uniform look of the pages (defines the header
   and the style sheet)."
  `(with-html-output-to-string
    (*standard-output* nil :prologue t :indent t)
    (:html :lang "en"
           (:head 
            (:meta :charset "utf-8")
            (:title ,title)
            (:link :type "text/css" 
                   :rel "stylesheet"
                   :href "/retro.css"))
           (:body 
            (:div :id "header" ; Retro games header
                  (:img :src "/logo.jpg" 
                        :alt "Commodore 64" 
                        :class "logo")
                  (:span :class "strapline" 
                         "Vote on your favourite Retro Game"))
            ,@body))))

(defmacro standard-page ((&key title script) &body body)
  "All pages on the Retro Games site will use the following macro;
   less to type and a uniform look of the pages (defines the header
   and the stylesheet).
   The macro also accepts an optional script argument. When present, the
   script form is expected to expand into valid JavaScript."
  `(with-html-output-to-string
    (*standard-output* nil :prologue t :indent t)
    (:html :lang "en"
           (:head 
            (:meta :charset "utf-8")
            (:title ,title)
            (:link :type "text/css" 
                   :rel "stylesheet"
                   :href "/retro.css")
            ,(when script
               `(:script :type "text/javascript"
                         (str ,script))))
           (:body 
            (:div :id "header" ; Retro games header
                  (:img :src "/logo.jpg" 
                        :alt "Commodore 64" 
                        :class "logo")
                  (:span :class "strapline" 
                         "Vote on your favourite Retro Game"))
            ,@body))))

;; HTML
;; ====

;; The functions responsible for generating the actual pages of our app go here.
;; We use the Hunchentoot macro define-easy-handler to automatically
;; push our uri to the dispatch table of the server and associate the
;; request with a function that will handle it.

(define-easy-handler (retro-games :uri "/retro-games") ()
  (standard-page (:title "Top Retro Games")
     (:h1 "Vote on your all time favourite retro games!")
     (:p "Missing a game? Make it available for votes " (:a :href "new-game" "here"))
     (:h2 "Current stand")
     (:div :id "chart" ; Used for CSS styling of the links.
       (:ol
	(dolist (game (games))
	 (htm  
	  (:li (:a :href (format nil "vote?name=~a" (escape-string ; avoid injection attacks
                                                     (name game))) "Vote!")
	       (fmt "~A with ~d votes" (name game) (votes game)))))))))

(define-easy-handler (new-game :uri "/new-game") ()
  (standard-page (:title "Add a new game"
                         :script (ps  ; client side validation
                                  (defvar add-form nil)
                                  (defun validate-game-name (evt)
                                    "For a more robust event handling
                                     mechanism you may want to consider
                                     a library (e.g. jQuery) that encapsulates
                                     all browser-specific quirks."
                                    (when (= (@ add-form name value) "")
                                      (chain evt (prevent-default))
                                      (alert "Please enter a name.")))
                                  (defun init ()
                                    (setf add-form (chain document
                                                          (get-element-by-id "addform")))
                                    (chain add-form
                                           (add-event-listener "submit" validate-game-name false)))
                                  (setf (chain window onload) init)))
     (:h1 "Add a new game to the chart")
     (:form :action "/game-added" :method "post" :id "addform"
            (:p "What is the name of the game?" (:br)
                (:input :type "text" :name "name" :class "txt"))
            (:p (:input :type "submit" :value "Add" :class "btn")))))

(define-easy-handler (game-added :uri "/game-added") (name)
  (unless (or (null name) (zerop (length name))) ; In case JavaScript is turned off.
    (add-game name))
  (redirect "/retro-games")) ; back to the front page

(define-easy-handler (vote :uri "/vote") (name)
  (when (game-stored? name)
    (vote-for (game-from-name name)))
  (redirect "/retro-games")) ; back to the front page

;; Alright, everything has been defined - launch Hunchentoot and have it
;; listen to incoming requests:
(publish-static-content)
(start-server 8080)
