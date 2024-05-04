;;; slime-print-buffer.el --- SLIME print buffers    -*- lexical-binding: t; -*-

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

;; SLIME print buffers

;;; Code:

(require 'cl-lib)
(require 'slime)
(require 'slime-breakpoints)

(defun slime-debug-print-next-expression-in-buffer ()
  "Instrument next expression to be debug printed in an Emacs buffer when evaluated.
The function at point is compiled with the extra debugging code.
Use `slime-compile-defun' on the function source code to recompile without the debugging stuff.
Requires cl-debug-print."
  (interactive)
  (let* (expr
         (source-with-print
          (slime--wrap-next-expression
           (lambda (exp)
             (setq expr exp)
             (format "(swank-print-buffer:prn %s)" exp)))))
    (slime--async-compile-string
     source-with-print 0
     (lambda (_result)
       (display-message-or-buffer (format "Instrumented for debug printing: %s" expr))))))

(defun slime-debug-print-last-expression-in-buffer ()
  "Instrument last expression to be debug printed in an Emacs buffer when evaluated.
The function at point is compiled with the extra debugging code.
Use `slime-compile-defun' on the function source code to recompile without the debugging stuff.
Requires cl-debug-print."
  (interactive)
  (let* (expr
         (source-with-print
          (slime--wrap-last-expression
           (lambda (exp)
             (setq expr exp)
             (format "(swank-print-buffer:prn %s)" exp)))))
    (slime--async-compile-string
     source-with-print 0
     (lambda (_result)
       (display-message-or-buffer (format "Instrumented for debug printing: %s" expr))))))

(defun slime-print-buffer-add-hooks ()
  (add-hook 'slime-event-hooks 'slime-print-buffer-event-handler))

(defun slime-print-buffer-event-handler (event)
  (slime-dcase event
    ((:spb/print printed)
     (let ((buffer (get-buffer-create "*slime-print*")))
       (with-current-buffer buffer
         (goto-char (point-max))
         (insert (cl-getf printed :expr))
         (insert " => ")
         (insert-text-button (cl-getf printed :value)
                             'action (lambda (_btn)
                                       (slime-eval `(swank-print-buffer::inspect-printed ,(cl-getf printed :id))))
                             'follow-link t
                             'help-echo "Inspect")
         (newline)
         (unless (get-buffer-window)
           (display-buffer buffer))
         (unless (eq buffer (window-buffer (selected-window))) ;; buffer has focus
           (set-window-point (get-buffer-window buffer) (point-max)))))
     t)
    (t nil)))

(add-hook 'slime-event-hooks 'slime-print-buffer-event-handler)

(define-slime-contrib slime-print-buffer
  "Dedicated buffer for print output.
Interactive. Features inspection of printed values via SLIME inspector."
  (:authors "Mariano Montone <marianomontone@gmail.com>")
  (:license "GPL")
  (:slime-dependencies slime-buffer-streams)
  (:swank-dependencies swank-print-buffer))

(provide 'slime-print-buffer)
;;; slime-print-buffer.el ends here
