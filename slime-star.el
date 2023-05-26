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
(require 'inline-message)

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

;; -- Highlight expression before evaluating it ----------------------------

(defun slime-last-expression-region ()
  "Return last expression at point, and its region."
  (let (start end)
    (cl-values
     (buffer-substring-no-properties
      (save-excursion (backward-sexp) (setq start (point)))
      (setq end (point)))
     start end)))

(defun slime-highlight-last-expression ()
  "Highlight last expression."
  (cl-multiple-value-bind (exp start end)
      (slime-last-expression-region)
    (slime-flash-region start end)))

(advice-add 'slime-eval-last-expression
	    :before #'slime-highlight-last-expression)

;; -- Display evaluation results in buffer -------------------

;; This is not implemented very prettily.
;; Function advice is used to hook into SLIME machinery for displaying evaluation results.

(defcustom slime-star-display-eval-result-in-buffer nil
  "Temporarily print SLIME evaluations at current position in buffer."
  :type 'boolean
  :group 'slime-star)

(defvar slime-current-buffer nil
  "Saved value of `current-buffer' before SLIME switches to using the connection buffer as `current-buffer'.
This is used by the `inline-message' display functions, as it needs to know the user's buffer.")

(defun slime-star-save-current-buffer (form)
  "Save current buffer and call `slime-eval-with-transcript'."
  (setq slime-current-buffer (current-buffer)))

(defun slime-star-display-eval-result (value)
  "Maybe show evaluation result in buffer too."
  (when slime-star-display-eval-result-in-buffer
    (if (buffer-live-p slime-current-buffer)
	(with-current-buffer slime-current-buffer
	  (inline-message value))
      (slime-message "%s" value))))

(advice-add 'slime-eval-with-transcript
	    :before #'slime-star-save-current-buffer)

(advice-add 'slime-display-eval-result
	    :after #'slime-star-display-eval-result)

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
  (:slime-dependencies quicklisp-systems quicklisp-apropos quicksearch slime-help system-browser-cl slime-breakpoints slime-stream-inspector)
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
