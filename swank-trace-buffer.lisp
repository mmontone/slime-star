(defpackage swank-trace-buffer
  (:use :cl))

(in-package :swank-trace-buffer)

;; Code that uses a plain slime stream

(defvar *trace-stream* (swank-buffer-streams:make-buffer-output-stream :trace))

(defun write-trace-to-stream (trace-entry)
  (princ (swank-trace-dialog::parent-of trace-entry) *trace-stream*)
  (format *trace-stream* "(~a ~s)"
          (swank-trace-dialog::spec-of trace-entry)
          (swank-trace-dialog::args-of trace-entry))
  (terpri *trace-stream*)
  (format *trace-stream* "=> ~s" (swank-trace-dialog::retlist-of trace-entry))
  (terpri *trace-stream*)
  (finish-output *trace-stream*))

(pushnew 'write-trace-to-stream swank-trace-dialog::*after-trace-hooks*)

;; Code that sends special trace events to Emacs
(defun send-trace-event (trace)
  (swank::with-connection (swank::*emacs-connection*)
    (with-simple-restart
        (abort "Abort sending trace to Emacs.")
      (swank::send-to-emacs
       `(:stb/trace ,(swank-trace-dialog::describe-trace-for-emacs
                      trace))))))

(pushnew 'send-trace-event swank-trace-dialog::*after-trace-hooks*)

(pushnew 'handle-trace-event swank::*event-hook*)

(defun handle-trace-event (connection event)
  (swank::dcase event
    ((:stb/trace ev)
     (swank::encode-message event (swank::current-socket-io))
     t)
    (t nil)))
