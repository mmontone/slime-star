(require 'slime)

(defvar slime--highlighted nil)

(defun slime-stepper--highlight-sexp (&optional start end)
  "Highlight the first sexp after point."
  (slime--delete-highlights)
  (let* ((start (or start (point)))
	 (end (or end (save-excursion (ignore-errors (forward-sexp)) (point))))
	 (overlay (make-overlay start end)))
    (overlay-put overlay 'face 'secondary-selection)
    (setq slime--highlighted overlay)))

(defun slime--delete-highlights (&optional _args)
  (when slime--highlighted
    (delete-overlay slime--highlighted)
    (setq slime--highlighted nil)))


(defun slime-stepper--install ()
  "Use custom highlighter when stepping."
  (advice-add 'slime-highlight-sexp :override 'slime-stepper--highlight-sexp)
  (advice-add 'sldb-quit :after 'slime--delete-highlights)
  (advice-add 'sldb-abort :after 'slime--delete-highlights)
  (advice-add 'sldb-continue :after 'slime--delete-highlights)
  (advice-add 'sldb-invoke-restart :after 'slime--delete-highlights)
  (advice-add 'slime-display-eval-result :after (lambda (x) (slime--delete-highlights))))

(provide 'slime-stepper)
