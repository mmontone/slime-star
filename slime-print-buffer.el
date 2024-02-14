(require 'slime)
(require 'cl-lib)

(defun slime-print-buffer-add-hooks ()
  (add-hook 'slime-event-hooks 'slime-print-buffer-event-handler))

(defun slime-print-buffer-event-handler (event)
  (slime-dcase event
    ((:spb/print printed)
     (let ((buffer (get-buffer-create "*slime-print*")))
       (with-current-buffer buffer
         (insert (prin1-to-string key))
         (insert " => ")
         (insert (prin1-to-string value))
         (newline)
         (unless (get-buffer-window)
           (display-buffer buffer))))
     t)
    (t nil)))
