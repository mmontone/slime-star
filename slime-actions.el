;;; slime-actions.el --- Define Lisp actions and make them appear as Emacs commands          -*- lexical-binding: t; -*-

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

;; Define Lisp actions and make them appear as Emacs commands.

;;; Code:

(require 'slime)

(defun slime-run-action ()
  "Evaluate SLIME action."
  (interactive)
  (let ((actions (slime-eval'(slime-actions:actions-for-emacs))))
    (let ((action (completing-read "Run: " actions)))
      (let ((result-message (slime-eval `(slime-actions:run-action ',action))))
        (message result-message)))))

(provide 'slime-actions)
;;; slime-actions.el ends here
