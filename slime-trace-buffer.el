(require 'slime)
(require 'cl-lib)
(require 'anaphora)

(defun slime-trace-buffer-add-hooks ()
  (add-hook 'slime-event-hooks 'slime-trace-buffer-event-handler))

(defun slime-trace-buffer--get-buffer ()
  "Get buffer for traces."
  (aif (get-buffer "*slime-trace*")
      it
    (let ((buffer (generate-new-buffer "*slime-trace*")))
      (with-current-buffer buffer
        ;; Initialize here
        buffer))))

(defgroup slime-trace-buffer nil
  "SLIME Trace Buffer settings."
  :group 'slime)

(defcustom slime-trace-buffer-indent 3
  "Indentation in SLIME Trace Buffer."
  :type 'integer
  :group 'slime-trace-buffer)

(defun slime-trace-buffer--insert-trace-enter (trace)
  (goto-char (point-max))
  (cl-labels ((indent ()
                      (dotimes (l (* (cl-getf trace ':level)
                                     slime-trace-buffer-indent))
                        (insert " ")))
              (indsert (string)
                       (indent)
                       (insert string)))
    (let ((pos (point))
          (part 0))
      (indsert (prin1-to-string (cl-getf trace ':level)))
      (insert ": ")
      (insert (prin1-to-string (cl-getf trace ':spec)))
      (newline)
      (dolist (arg (cl-getf trace ':args))
        (indsert "> ")
        (insert arg)
        (incf part)
        (newline))
      ;;(goto-char pos)
      ;;(outline-hide-subtree)
      ;;(outline-hide-leaves)
      )))

(defun slime-trace-buffer--insert-trace-exit (trace)
  (cl-labels ((indent ()
                      (dotimes (l (* (cl-getf trace ':level)
                                     slime-trace-buffer-indent))
                        (insert " ")))
              (indsert (string)
                       (indent)
                       (insert string)))
    (goto-char (point-max))
    (let ((part (length (cl-getf trace ':args))))
      (indsert (prin1-to-string (cl-getf trace ':level)))
      (insert ": ")
      (insert (prin1-to-string (cl-getf trace ':spec)))
      (newline)
      (dolist (retval (cl-getf trace ':retlist))
        (indsert "< ")
        (insert retval)
        (incf part)
        (newline)))))

(defun slime-trace-buffer-event-handler (event)
  (slime-dcase event
    ((:stb/trace-enter trace)
     (let ((buffer (slime-trace-buffer--get-buffer)))
       (with-current-buffer buffer
         (slime-trace-buffer--insert-trace-enter trace)
         (newline)
         (unless (get-buffer-window)
           (display-buffer buffer))))
     t)
    ((:stb/trace-exit trace)
     (let ((buffer (slime-trace-buffer--get-buffer)))
       (with-current-buffer buffer
         (slime-trace-buffer--insert-trace-exit trace)
         (newline)
         (unless (get-buffer-window)
           (display-buffer buffer))))
     t)
    (t nil)))

;; (setq-local outline-minor-mode-use-buttons t)

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
