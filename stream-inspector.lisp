(defpackage :stream-inspector
  (:use :cl)
  (:export
   #:get-lisp-obj-by-address
   #:get-lisp-obj-by-persistent-pointer
   #:*persistent-pointers*))

(in-package :stream-inspector)

(defvar *printed-counter* 0)
(defvar *printed-objects* (make-hash-table :weakness :value))
(defvar *persistent-pointers* nil)

;; When *PERSISTENT-POINTERS* is enabled, CLOS objects are printed with a persistent pointer at the beggining.
;; That pointer can be later used to find the object by its printed representation.
;; Unlike the get-lisp-obj-by-address approach, this is portable and also immune to garbage collection moves.

(defmethod print-object :around ((obj standard-object) stream)
  (when *persistent-pointers*
    (let ((printed-id (incf *printed-counter*)))
      (setf (gethash printed-id *printed-objects*) obj)
      (format stream "#~a" printed-id)))
  (call-next-method))

(defmethod print-object :around ((obj condition) stream)
  (when *persistent-pointers*
    (let ((printed-id (incf *printed-counter*)))
      (setf (gethash printed-id *printed-objects*) obj)
      (format stream "#~a" printed-id)))
  (call-next-method))

(defun get-lisp-obj-by-persistent-pointer (pointer)
  (when *persistent-pointers*
    (gethash pointer *printed-objects*)))

#+sbcl
(defun get-lisp-obj-by-address (address)
  (sb-kernel:make-lisp-obj address))

(provide :stream-inspector)
