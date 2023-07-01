;;; slime-critic.el --- SLIME extension for LISP-CRITIC   -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Mariano Montone

;; Author: Mariano Montone(require 'slime) <marianomontone@gmail.com>
;; Keywords: tools, lisp, extensions

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

;; SLIME extension for LISP-CRITIC

;;; Code:

(require 'slime)

(defun slime-critic--show-critiques (critiques)
  "Show the Lisp CRITIQUES."
  (let ((buffer (get-buffer-create "*slime-critic*")))
    (with-current-buffer buffer
      (dolist (critique critiques)
        (insert (cdr critique))
        (newline 2))
      (display-buffer buffer))))

(defun slime-critic-critique-file (file)
  "Critique the FILE."
  (interactive "fCritique file: ")
  (slime-eval-async `(slime-critic:critique-file ,file)
    (lambda (critiques)
      (slime-critic--show-critiques critiques))))

(provide 'slime-critic)

;;; slime-critic.el ends here
