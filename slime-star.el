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
(require 'slime-stepper)
(require 'slime-toolbars)
(require 'slime-star-commands)

(defgroup slime-star nil
  "SLIME Star (SLIME extensions)."
  :group 'slime)

(defcustom slime-star-use-custom-stepper-highlighter nil
  "Use custom stepper highlighter when enabled."
  :type 'boolean
  :group 'slime-star)

(defcustom slime-star-use-toolbars nil
  "Use custom toolbars for SLIME buffers."
  :type 'boolean
  :group 'slime-star)

(defun slime-star--setup-key-bindings ()
  (define-key sldb-mode-map "Q" 'sldb-kill-all-buffers))

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
  (:slime-dependencies quicklisp-systems quicksearch slime-help system-browser-cl slime-breakpoints slime-stream-inspector)
  (:swank-dependencies slime-star)
  (:on-load
   ;; setup key bindings
   (slime-star--setup-key-bindings)
   ;; add submenu to SLIME menu
   (slime-star--setup-menus)
   (add-hook 'slime-connected-hook
	     (lambda ()
	       (when slime-star-use-toolbars
		 (slime-toolbars-setup-tool-bars))
	       (when slime-star-use-custom-stepper-highlighter
		 (slime-stepper--install))))
   (advice-add 'slime-load-contribs :before #'slime-star--add-swank-path)
   ))

(provide 'slime-star)
