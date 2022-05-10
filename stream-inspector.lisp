(defpackage :stream-inspector
  (:use :cl)
  (:export #:get-lisp-obj-by-address))

(in-package :stream-inspector)

#+sbcl
(defun get-lisp-obj-by-address (address)
  (sb-kernel:make-lisp-obj address))

(provide :stream-inspector)
