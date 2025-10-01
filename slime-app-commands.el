;;; slime-app-commands.el --- Define application specific commands Lisp side, then run them from Emacs.          -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Mariano Montone

;; Author: Mariano Montone <marianomontone@gmail.com>
;; Keywords: tools
;; Version: 0.1

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

;; Define application specific commands Lisp side, then run them from Emacs.

;; Bind menu key:
;; (slime-app-commands-bind-menu-key)

;;; Code:

(require 'slime)

(defun slime-app-commands-run ()
  "Evaluate SLIME command."
  (interactive)
  (let ((commands (slime-eval'(slime-app-commands:commands-for-emacs))))
    (let ((command (completing-read "Run: " (mapcar #'downcase commands))))
      (let ((result-message (slime-eval `(slime-app-commands:run-command ',command))))
        (message result-message)))))

(defun slime-app-commands-bind-menu-key (&optional key)
  "Bind menu KEY to `slime-app-commands-run'."
  (let ((key (or key "<menu>")))
    (add-hook 'lisp-mode-hook
              (lambda ()
                (local-set-key (kbd key) 'slime-app-commands-run)))
    (add-hook 'slime-repl-mode-hook
              (lambda ()
                (local-set-key (kbd key) 'slime-app-commands-run)))))

(provide 'slime-app-commands)
;;; slime-app-commands.el ends here
