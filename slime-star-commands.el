(require 'slime)

(defun slime-room ()
  "Show Common Lisp ROOM information in an Emacs buffer."
  (interactive)
  (let ((room-str
	 (slime-eval `(cl:with-output-to-string (cl:*standard-output*)
						(cl:room)))))
    (let ((buffer (get-buffer-create "*slime-room*")))
      (with-current-buffer buffer
	(insert room-str)
	(display-buffer buffer)))))

(defun slime-scratch ()
  "Open the equivalent of an Emacs *scratch* buffer, for Common Lisp/SLIME."
  (interactive)
  (let ((buffer (get-buffer-create "*slime-scratch*")))
    (with-current-buffer buffer
      (lisp-mode)
      (switch-to-buffer buffer))))

(defun sldb-kill-all-buffers ()
  "Kill all SLDB buffers."
  (interactive)
  (dolist (buf (sldb-buffers))
    (kill-buffer buf)))

(defun sldb-show-all-frames-details ()
  "Show details of all frames"
  (interactive)
  (let ((inhibit-read-only t)
        (inhibit-point-motion-hooks t))
    (sldb-beginning-of-backtrace)
    ;;(while (get-text-property (point) 'frame)
    ;;  (sldb-show-frame-details)
    ;;  (sldb-forward-frame))

    (dotimes (i 5)
      (when (get-text-property (point) 'frame)
	(sldb-show-frame-details)
	(sldb-forward-frame)))))

(provide 'slime-star-commands)
