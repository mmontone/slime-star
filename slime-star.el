;;; slime-star --- SLIME with augmented features.  -*- lexical-binding: t -*-

;; Copyright (C) 2022 Mariano Montone

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;; Code:

(require 'slime)

(defun slime-room ()
  "Show Common Lisp ROOM information in an Emacs buffer."
  (interactive)
  (let ((room-str
	 (slime-eval `(cl:with-output-to-string (cl:*standard-output*)
						(cl:room)))))
    (let ((buffer (get-buffer-create "*slime-room*")))
      (with-current-buffer buffer
	(insert room-str)
	(display-buffer buffer)))))

(defun slime-scratch ()
  "Open the equivalent of an Emacs *scratch* buffer, for Common Lisp/SLIME."
  (interactive)
  (let ((buffer (get-buffer-create "*slime-scratch*")))
    (with-current-buffer buffer
      (lisp-mode)
      (switch-to-buffer buffer))))

(defun slime-star-setup-key-bindings ()
  )

(defun slime-star--setup-menus ()
  (easy-menu-add-item 'menubar-slime nil
		      '("Tools"
			["System Browser" lisp-system-browser]
			["Quicklisp Systems" quicklisp-systems]
			["Quicksearch" quicksearch])
		      "Documentation"))

(defvar slime-star--load-path (file-name-directory load-file-name))

(defun slime-star--add-swank-path ()
  (slime-eval `(cl:progn (cl:push ,slime-star--load-path swank::*load-path*) nil)))

(define-slime-contrib slime-star
  "SLIME with extra extensions preinstalled."
  (:authors "Mariano Montone")
  (:license "GPL")
  (:slime-dependencies quicklisp-systems quicksearch slime-help system-browser slime-breakpoints)
  (:swank-dependencies slime-star)
  (:on-load
   ;; setup key bindings
   (slime-star-setup-key-bindings)
   ;; add submenu to SLIME menu
   (slime-star--setup-menus)

   (advice-add 'slime-load-contribs :before #'slime-star--add-swank-path)
	     
   ))

(provide 'slime-star)
