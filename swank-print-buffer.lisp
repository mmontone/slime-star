(defpackage swank-print-buffer
  (:use :cl)
  (:nicknames :swankpb)
  (:export #:prn))

(in-package :swank-print-buffer)

(defvar *printed* (make-array 1000 :fill-pointer 0
                                   :adjustable t))
(defvar *record-printed-p* t)

(defstruct printed
  expr value)

(defmacro prn (expr)
  `(let ((printed (make-printed :expr ',expr :value ,expr)))
     (when *record-printed-p*
       (vector-push-extend printed *printed*))
     (send-print-event printed)))

;; Code that uses a plain slime stream

(defvar *print-stream* (swank-buffer-streams:make-buffer-output-stream :print))

(defun print-to-stream (expr value)
  (princ expr *print-stream*)
  (write-string " => " *print-stream*)
  (princ value *print-stream*)
  (terpri *print-stream*)
  (finish-output *print-stream*))

;; Code that sends special print events to Emacs

(defun describe-for-emacs (printed)
  (list :expr (prin1-to-string (printed-expr printed))
        :value (princ-to-string (printed-value printed))))

(defun send-print-event (printed)
  (swank::with-connection (swank::*emacs-connection*)
    (with-simple-restart
        (abort "Abort sending trace to Emacs.")
      (swank::send-to-emacs
       `(:spb/print ,(describe-for-emacs printed))))))

(pushnew 'handle-print-event swank::*event-hook*)

(defun handle-print-event (connection event)
  (declare (ignore connection))
  (swank::dcase event
    ((:spb/print ev)
     (declare (ignore ev))
     (swank::encode-message event (swank::current-socket-io))
     t)
    (t nil)))

(defun find-printed (id)
  (when (<= 0 id (1- (length *printed*)))
    (aref *printed* id)))

(defun inspect-printed (id)
  (swank::inspect-in-emacs (find-printed id)))
