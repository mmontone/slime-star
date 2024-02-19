(defpackage swank-trace-buffer
  (:use :cl)
  (:export :*print-width*))

(in-package :swank-trace-buffer)

(defvar *print-width* nil)

;; Example: use swank-buffer-streams as *trace-output*

;; (setf *trace-output* (swank-buffer-streams:make-buffer-output-stream :trace))

;; Code that uses a plain slime stream

;; (defvar *trace-stream* (swank-buffer-streams:make-buffer-output-stream :trace))

;; (defun write-trace-to-stream (trace-entry)
;;   (princ (swank-trace-dialog::parent-of trace-entry) *trace-stream*)
;;   (format *trace-stream* "(~a ~s)"
;;           (swank-trace-dialog::spec-of trace-entry)
;;           (swank-trace-dialog::args-of trace-entry))
;;   (terpri *trace-stream*)
;;   (format *trace-stream* "=> ~s" (swank-trace-dialog::retlist-of trace-entry))
;;   (terpri *trace-stream*)
;;   (finish-output *trace-stream*))

;; (pushnew 'write-trace-to-stream swank-trace-dialog::*after-trace-hooks*)

;; Code that sends special trace events to Emacs

(defun trace-level (trace)
  (let ((parent (swank-trace-dialog::parent-of trace)))
    (if (null parent) 0
        (1+ (trace-level parent)))))

(defun write-object (obj)
  (swank::to-line obj *print-width*))

(defun describe-trace-for-emacs (trace)
  (list :id (swank-trace-dialog::id-of trace)
        :parent (and (swank-trace-dialog::parent-of trace)
                     (swank-trace-dialog::id-of (swank-trace-dialog::parent-of trace)))
        :spec (swank-trace-dialog::spec-of trace)
        :args (mapcar #'write-object (swank-trace-dialog::args-of trace))
        :retlist (mapcar #'write-object (swank::ensure-list (swank-trace-dialog::retlist-of trace)))
        :level (trace-level trace)))

(defun send-trace-enter-event (trace)
  (swank::with-connection (swank::*emacs-connection*)
    (with-simple-restart
        (abort "Abort sending trace to Emacs.")
      (swank::send-to-emacs
       (list :stb/trace-enter (describe-trace-for-emacs trace))))))

(defun send-trace-exit-event (trace)
  (swank::with-connection (swank::*emacs-connection*)
    (with-simple-restart
        (abort "Abort sending trace to Emacs.")
      (swank::send-to-emacs
       (list :stb/trace-exit (describe-trace-for-emacs trace))))))

(pushnew 'send-trace-enter-event swank-trace-dialog::*before-trace-hooks*)
(pushnew 'send-trace-exit-event swank-trace-dialog::*after-trace-hooks*)

;; Handle trace events from SWANK side
(pushnew 'handle-trace-event swank::*event-hook*)

(defun handle-trace-event (connection event)
  (swank::dcase event
    ((:stb/trace-enter ev)
     (swank::encode-message event (swank::current-socket-io))
     t)
    ((:stb/trace-exit ev)
     (swank::encode-message event (swank::current-socket-io))
     t)
    (t nil)))

(defun inspect-trace (id)
  (swank::inspect-in-emacs (swank-trace-dialog::find-trace id))
  "ok")

(defun inspect-trace-part (id index type)
  (let ((trace (swank-trace-dialog::find-trace id)))
    (swank::inspect-in-emacs (swank-trace-dialog::find-trace-part id index type))
    "ok"))

(provide :swank-trace-buffer)
