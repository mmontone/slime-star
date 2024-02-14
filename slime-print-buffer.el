(require 'slime)
(require 'cl-lib)

(defun slime-print-buffer-add-hooks ()
  (add-hook 'slime-event-hooks 'slime-print-buffer-event-handler))

(defun slime-print-buffer-event-handler (event)
  (slime-dcase event
    ((:spb/print printed)
     (let ((buffer (get-buffer-create "*slime-print*")))
       (with-current-buffer buffer
         (insert (cl-getf printed :expr))
         (insert " => ")
         (insert (cl-getf printed :value))
         (newline)
         (unless (get-buffer-window)
           (display-buffer buffer))))
     t)
    (t nil)))
