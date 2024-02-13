(require 'slime)
(require 'cl-lib)

(defun slime-trace-buffer-add-hooks ()
  (add-hook 'slime-event-hooks 'slime-trace-buffer-event-handler))

(defun slime-trace-buffer-event-handler (event)
  (slime-dcase event
    ((:stb/trace trace)
     (let ((buffer (get-buffer-create "*slime-trace*")))
       (with-current-buffer buffer
       (insert (prin1-to-string trace))
       (newline)
       (unless (get-buffer-window)
         (display-buffer buffer))))
     t)
    (t nil)))
