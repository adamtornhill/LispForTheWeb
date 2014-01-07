(defpackage :retro-games
  (:use :cl :cl-who :hunchentoot :parenscript :cl-mongo))

(in-package :retro-games)

;; Domain model
;; ============

;; A simple domain model of our games together with
;; access methods.

(defclass game ()
  ((name  :reader   name 
          :initarg  :name)
   (votes :accessor votes
          :initarg :votes ; when read from persistent storage
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

;; Pre-requisite: a mongod daemon process runs on localhost
;; using the default port.
;; Here we establish a connection to the database games that
;; we'll use for all storage:
(cl-mongo:db.use "games")

;; We store all game documents in the following collection:
(defparameter *game-collection* "game")

;; We encapsulate all knowledge of the concrete storage
;; medium in the following functions:

(defun game->doc (game)
  ($ ($ "name" (name game))
     ($ "votes" (votes game))))

(defun doc->game (game-doc)
  (make-instance 'game :name (get-element "name" game-doc)
                       :votes (get-element "votes" game-doc)))

(defmethod vote-for :after (game)
  "In this method we update the votes in the persistent storage.
   An after method in CLOS gives us an Observer-like behaviour;
   once the primary method has run, CLOS invokes our after method."
  (let ((game-doc (game->doc game)))
    (db.update *game-collection* ($ "name" (name game)) game-doc)))

(defun game-from-name (name)
  "Queries the database for a game matching the
   given name.
   Note that db.find behaves like Mongo's findOne by default, so
   when we found-games we know there can be only one."
  (let ((found-games (docs (db.find *game-collection* ($ "name" name)))))
    (when found-games
      (doc->game (first found-games)))))

(defun game-stored? (name)
  (game-from-name name))

(defun games ()
  "Returns a sequence of all games, sorted on
   their number of votes in descending order.
   The implementation is straightforwards since
   cl-mongo provides a db.sort macro. We just need
   to remember that we get a lazy sequence back and
   have to realize it (iterate to the end) using iter."
  (mapcar #'doc->game
          (docs (iter (db.sort *game-collection* :all
                                                 :field "votes"
                                                 :asc nil)))))

(defun add-game (name)
  "Add a game with the given name to the database.
   In this version we don't check for duplicates."
  (let ((game (make-instance 'game :name name)))
    (db.insert *game-collection* (game->doc game))))

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
