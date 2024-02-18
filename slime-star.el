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
(require 'sldb-show-frame-local)
(require 'inline-message)
(require 'info)
(require 'info-look)

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

(defcustom slime-star-show-frame-local-on-cursor-move nil
  "Show frame local in debugger when cursor moves."
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

(defun slime-toggle-output-buffer ()
  "Toggle sending output to a buffer."
  (interactive)
  (let ((enabled? (slime-eval `(slime-star:toggle-send-output-to-buffer))))
    (message (if enabled?
                 "Output buffer enabled"
               "Output buffer disabled"))))

(defun slime-toggle-trace-buffer ()
  "Toggle sending traces to a buffer."
  (interactive)
  (let ((enabled? (slime-eval `(slime-star:toggle-send-trace-to-buffer))))
    (message (if enabled?
                 "Trace buffer enabled"
               "Trace buffer disabled"))))

(defun slime-toggle-error-buffer ()
  "Toggle sending errors to a buffer."
  (interactive)
  (let ((enabled? (slime-eval `(slime-star:toggle-send-error-to-buffer))))
    (message (if enabled?
                 "Error buffer enabled"
               "Error buffer disabled"))))

(defun slime-star--setup-key-bindings ()
  (define-key sldb-mode-map "Q" 'sldb-kill-all-buffers))

(defun slime-star--setup-menus ()
  (easy-menu-add-item 'menubar-slime nil
                      '("Tools"
                        ["System Browser" lisp-system-browser]
                        ["Quicklisp Systems" quicklisp-systems]
                        ["Quicksearch" quicksearch])
                      "Documentation")
  (easy-menu-add-item 'menubar-slime nil
                      '("Stream buffers"
                        ["Toggle output buffer" slime-toggle-output-buffer]
                        ["Toggle error buffer" slime-toggle-error-buffer]
                        ["Toggle trace buffer" slime-toggle-trace-buffer])
                      "Documentation"))

(defvar slime-star--load-path (file-name-directory load-file-name))

;;--- ANSI Common Lisp spec in Info format ------------------------

(add-to-list 'Info-default-directory-list (concat slime-star--load-path "info"))
(setq Info-directory-list nil)
(info-initialize)

(info-lookup-add-help
 :mode 'lisp-mode
 :regexp "[^][()'\" \t\n]+"
 :ignore-case t
 :doc-spec '(("(ansicl)Symbol Index" nil nil nil)))

(defun slime-star-ansicl-lookup (symbol-name)
  (info-lookup-symbol (slime-cl-symbol-name symbol-name) 'lisp-mode))

(setq slime-help-ansicl-lookup-function 'slime-star-ansicl-lookup)

;;--- SLIME contrib -----------------------------------------------

(defun slime-star--add-swank-path ()
  (slime-eval `(cl:progn (cl:push ,slime-star--load-path swank::*load-path*) nil)))

(define-slime-contrib slime-star
  "SLIME with extra extensions preinstalled."
  (:authors "Mariano Montone")
  (:license "GPL")
  (:slime-dependencies quicklisp-systems quicklisp-apropos quicksearch slime-help system-browser-cl slime-breakpoints slime-stream-inspector sldb-source-eval slime-critic slime-trace-buffer slime-buffer-streams)
  (:swank-dependencies slime-star)
  (:on-load
   ;; setup key bindings
   (slime-star--setup-key-bindings)
   ;; add submenu to SLIME menu
   (slime-star--setup-menus)
   (when slime-star-show-frame-local-on-cursor-move
     (sldb-show-frame-local-on-cursor-move))
   (add-hook 'slime-connected-hook
             (lambda ()
               (when slime-star-use-toolbars
                 (slime-toolbars-setup-tool-bars))
               (when slime-star-use-custom-stepper-highlighter
                 (slime-stepper--install))))
   (advice-add 'slime-load-contribs :before #'slime-star--add-swank-path)
   ))

(provide 'slime-star)
