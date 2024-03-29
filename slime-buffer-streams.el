(eval-and-compile
  (require 'slime))

(define-slime-contrib slime-buffer-streams
  "Lisp streams that output to an emacs buffer"
  (:authors "Ed Langley <el-github@elangley.org>")
  (:license "GPL")
  (:swank-dependencies swank-buffer-streams))

(defslimefun slime-make-buffer-stream-target (thread name)
  (message "Created stream buffer: %s" name)
  (slime-buffer-streams--get-target-marker name)
  `(:stream-target-created ,thread ,name))

(defun slime-buffer-streams--get-target-name (target)
  (format "*%s*" (slime-cl-symbol-name target)))

(defvar-local slime-buffer-stream-target nil)

;; TODO: tell backend that the buffer has been closed, so it can close
;;       the stream
(defun slime-buffer-streams--cleanup-markers ()
  (when slime-buffer-stream-target
    (message "Removed stream buffer: %s" slime-buffer-stream-target)
    (remhash slime-buffer-stream-target slime-output-target-to-marker)))

(defun slime-buffer-streams--get-target-marker (target)
  (or (gethash target slime-output-target-to-marker)
      (let ((buffer (get-buffer-create (slime-buffer-streams--get-target-name target))))
        (with-current-buffer buffer
          (setq slime-buffer-stream-target target)
          (add-hook 'kill-buffer-hook 'slime-buffer-streams--cleanup-markers)
          (setf (gethash target slime-output-target-to-marker)
                (point-marker))
          (when (not (get-buffer-window buffer))
            (display-buffer buffer))))))

(provide 'slime-buffer-streams)
