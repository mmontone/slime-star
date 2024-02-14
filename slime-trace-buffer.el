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

;; (setq outline-minor-mode-use-buttons t)

;; (setq-local outline-regexp "\s*[0-100]\:")

;; (defun insert-test ()
;;   (let ((pos (point)))
;;     (insert "0: lalal")
;;     (newline)
;;     (insert "> 34343")
;;     (newline)
;;     (insert "< aaa")
;;     (newline)
;;     (insert "  1: asfasdf")
;;     (newline)
;;     (insert "  < 'asdf")
;;     (newline)
;;     (goto-char pos)
;;     (outline-hide-subtree)
;;     (outline-hide-leaves)))

;; (setq-local header-line-format "[RET] open [i] inspect")
