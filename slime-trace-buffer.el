;;; slime-trace-buffer.el --- SLIME buffer for displaying traces  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Mariano Montone

;; Author: Mariano Montone <marianomontone@gmail.com>
;; Keywords: lisp, tools

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(provide 'slime-trace-buffer)
;;; slime-trace-buffer.el ends here

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

(defcustom slime-trace-buffer-use-buttons t
  "Use buttons in SLIME Trace buffers."
  :type 'boolean
  :group 'slime-trace-buffer)

(defun slime-trace-buffer--insert-button (label action &rest props)
  (if slime-trace-buffer-use-buttons
      (apply #'insert-text-button label
             'action (lambda (_btn) (funcall action))
             'follow-link t
             props)
    (insert label)))

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
      (indent)
      (slime-trace-buffer--insert-button
       (prin1-to-string (cl-getf trace ':level))
       (lambda () (slime-trace-buffer--inspect-trace (cl-getf trace ':id)))
       'help-echo "Inspect trace")
      (insert ": ")
      (insert (prin1-to-string (cl-getf trace ':spec)))
      (newline)
      (dolist (arg (cl-getf trace ':args))
        (indsert "< ")
        (slime-trace-buffer--insert-button
         arg (let ((trace-part part))
               (lambda () (slime-trace-buffer--inspect-trace-part (cl-getf trace ':id) trace-part :arg))))
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
    (let ((part 0))
      (indsert (prin1-to-string (cl-getf trace ':level)))
      (insert ": ")
      (insert (prin1-to-string (cl-getf trace ':spec)))
      (newline)
      (dolist (retval (cl-getf trace ':retlist))
        (indsert "> ")
        (slime-trace-buffer--insert-button
         retval (let ((trace-part part))
                  (lambda () (slime-trace-buffer--inspect-trace-part (cl-getf trace :id) trace-part :retval))))
        (incf part)
        (newline)))))

(defun slime-trace-buffer--inspect-trace (id)
  (slime-eval `(swank-trace-buffer::inspect-trace ,id)))

(defun slime-trace-buffer--inspect-trace-part (id index type)
  (slime-eval `(swank-trace-buffer::inspect-trace-part ,id ,index ',type)))

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
           (display-buffer buffer))
         (unless (eq buffer (window-buffer (selected-window))) ;; buffer has focus
           (set-window-point (get-buffer-window buffer) (point-max)))))
     t)
    (t nil)))

(add-hook 'slime-event-hooks 'slime-trace-buffer-event-handler)

(define-slime-contrib slime-trace-buffer
  "Dedicated buffer for trace output.
Interactive. Features inspection of traces arguments and return values using SLIME inspector."
  (:authors "Mariano Montone <marianomontone@gmail.com>")
  (:license "GPL")
  (:slime-dependencies slime-buffer-streams slime-trace-dialog)
  (:swank-dependencies swank-trace-buffer))

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
