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

(require 'slime)
(require 'cl-lib)

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

(provide 'slime-print-buffer)
;;; slime-print-buffer.el ends here
