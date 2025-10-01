(require :alexandria)

(defpackage :slime-app-commands
  (:use :cl)
  (:export #:defcommand
           #:commands-for-emacs
           #:run-command)
  (:documentation "Define commands Lisp side, then run them from Emacs."))

(in-package :slime-app-commands)

(defparameter *commands* (make-hash-table :test #'equalp))

(defmacro defcommand (name args &body body)
  `(flet ((,name ,args
            ,@body))
     (setf (gethash (symbol-name ',name) *commands*)
           #',name)))

(defun commands-for-emacs ()
  (alexandria:hash-table-keys *commands*))

(defun run-command (command-name)
  (let ((result (funcall (gethash command-name *commands*))))
    ;; Send the result message to Emacs
    (if (stringp result)
        result
        (princ-to-string result))))
