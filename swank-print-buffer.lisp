(defpackage swank-print-buffer
  (:use :cl)
  (:export #:prn))

(in-package :swank-print-buffer)

(defvar *printed* (make-array 1000 :fill-pointer 0
                                   :adjustable t))
(defvar *record-printed-p* t)

(defstruct printed
  key value)

(defmacro prn (code)
  (if *record-printed-p*
      `(let ((printed (make-printed :key ',code :value ,code)))
         (print-out ',code ,code))
      `(print-out ',code ,code)))

;; Code that uses a plain slime stream

(defvar *print-stream* (swank-buffer-streams:make-buffer-output-stream :print))

(defun print-to-stream (code value)
  (princ code *print-stream*)
  (write-string " => " *print-stream*)
  (princ value *print-stream*)
  (newline)
  (finish-output *trace-stream*))

;; Code that sends special print events to Emacs

(defun describe-for-emacs (printed)
  (list (cons 'code (printed-code printed))
        (cons 'value (printed-value printed))))

(defun send-print-event (printed)
  (swank::with-connection (swank::*emacs-connection*)
    (with-simple-restart
        (abort "Abort sending trace to Emacs.")
      (swank::send-to-emacs
       `(:spb/print ,(describe-for-emacs printed))))))

(pushnew 'handle-print-event swank::*event-hook*)

(defun handle-print-event (connection event)
  (swank::dcase event
    ((:spb/print ev)
     (swank::encode-message event (swank::current-socket-io))
     t)
    (t nil)))

(defun find-printed (id)
  (when (<= 0 id (1- (length *printed*)))
    (aref *printed* id)))

(defun inspect-printed (id)
  (swank::inspect-in-emacs (find-printed id)))