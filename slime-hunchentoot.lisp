(require :hunchentoot)

(defpackage :slime-hunchentoot
  (:use :cl :hunchentoot)
  (:export :list-acceptors))

(in-package :slime-hunchentoot)

(defvar *acceptors* nil)

(defmethod hunchentoot:start :before ((acceptor hunchentoot:acceptor))
  (pushnew acceptor *acceptors*))

(defun list-acceptors ()
  (mapcar (lambda (acceptor)
	    (list :name (acceptor-name acceptor)
		  :address (acceptor-address acceptor)
		  :port (acceptor-port acceptor)
		  :running (and (hunchentoot::acceptor-listen-socket acceptor) t)))
	  *acceptors*))

(provide :slime-hunchentoot)
