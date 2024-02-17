(require :def-properties (merge-pathnames "cl-def-properties/module.lisp" *load-pathname*))

(provide :slime-star)

(defpackage :slime-star
  (:use :cl)
  (:export
   #:toggle-send-output-to-buffer
   #:toggle-send-trace-to-buffer
   #:toggle-send-error-to-buffer
   #:toggle-send-debug))

(in-package :slime-star)

(defmacro def-toggle-stream-to-buffer (name stream-var target-name)
  `(defun ,name (&optional (enable? 'unset))
     (let ((enabled? nil)
           (original-stream ,stream-var))
       (labels ((enable ()
                  (setf ,stream-var
                        (swank-buffer-streams:make-buffer-output-stream ,target-name))
                  (setf enabled? t))
                (disable ()
                  (setf ,stream-var original-stream)
                  (setf enabled? nil))
                (toggle ()
                  (if enabled? (disable) (enable))))
         (if (eq enable? 'unset)
             (toggle)
             (if enable? (enable) (disable)))))))

(def-toggle-stream-to-buffer toggle-send-output-to-buffer *standard-output* :standard-output)
(def-toggle-stream-to-buffer toggle-send-error-to-buffer *error-output* :error-output)
(def-toggle-stream-to-buffer toggle-send-trace-to-buffer *trace-output* :trace-out)
