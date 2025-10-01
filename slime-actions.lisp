(defpackage :slime-actions
  (:use :cl)
  (:export #:defaction
           #:actions-for-emacs
           #:run-action)
  (:documentation "Define Lisp actions and make them appear as Emacs commands."))

(in-package :slime-actions)

(defparameter *actions* (make-hash-table :test #'equalp))

(defmacro defaction (name args &body body)
  `(flet ((,name ,args
            ,@body))
     (setf (gethash (symbol-name ',name) *actions*)
           #',name)))

(defun actions-for-emacs ()
  (alexandria:hash-table-keys *actions*))

(defun run-action (action-name)
  (let ((result (funcall (gethash action-name *actions*))))
    ;; Send the result message to Emacs
    (if (stringp result)
        result
        (princ-to-string result))))
