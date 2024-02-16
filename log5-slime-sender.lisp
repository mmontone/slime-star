(defpackage log5-slime-sender
  (:use :cl)
  (:export #:slime-sender))

(in-package :log5-slime-sender)

;; Code that uses a plain slime stream

(defclass slime-stream-sender (log5:stream-sender)
  ())

(defmethod log5:create-handle-message-context ((sender slime-stream-sender))
  `((stream (swank-buffer-streams:make-buffer-output-stream :log5))))

;; Code that sends special SLIME events to Emacs (features inspectable logs)

(defclass log-message ()
  ((id :initarg :id
       :accessor id-of)
   (sender :initarg :sender
           :accessor sender-of)
   (message :initarg :message
            :accessor message-of)
   (args :initarg :args
         :accessor args-of)))

(defclass slime-events-sender (log5:basic-sender)
  ((record-logs-p :type boolean
                  :accessor record-logs-p
                  :initform t)
   (logs :type array
         :accessor logs-of
         :initform (make-array 1000 :fill-pointer 0
                                    :adjustable t))))

(defmethod log5::create-message-for-sender ((sender slime-events-sender) message args)
  (when (record-logs-p sender)
    (let ((message (make-instance 'log-message
                                  :id (fill-pointer (logs-of sender))
                                  :sender sender
                                  :message message
                                  :args args)))
      (vector-push-extend message (logs-of sender))))
  (call-next-method))

(defun describe-for-emacs (log-message)
  (list :id (id-of log-message)
        :sender (log5::name  (sender-of log-message))
        :message (apply #'format (message-of log-message) (args-of log-message))
        :args (args-of log-message)))

(defun send-log-event (log-message)
  (swank::with-connection (swank::*emacs-connection*)
    (with-simple-restart
        (abort "Abort sending trace to Emacs.")
      (swank::send-to-emacs
       `(:log5/message ,(describe-for-emacs log-message))))))

(pushnew 'handle-log-event swank::*event-hook*)

(defun handle-log-event (connection event)
  (declare (ignore connection))
  (swank::dcase event
    ((:log5/message ev)
     (declare (ignore ev))
     (swank::encode-message event (swank::current-socket-io))
     t)
    (t nil)))

(defun find-sender (name)
  (error "TODO"))

(defun find-log-message (sender-name id)
  (let ((sender (find-sender sender-name)))
    (when (<= 0 id (1- (length (logs-of sender))))
      (aref (logs-of sender) id))))

(defun inspect-log-message (sender id)
  (swank::inspect-in-emacs (find-log-message sender id))
  "ok")
