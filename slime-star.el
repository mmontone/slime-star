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
(require 'slime-toolbars)

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

(defun sldb-kill-all-buffers ()
  "Kill all SLDB buffers."
  (interactive)
  (dolist (buf (sldb-buffers))
    (kill-buffer buf)))

(defun sldb-show-all-frames-details ()
  "Show details of all frames"
  (interactive)
  (let ((inhibit-read-only t)
        (inhibit-point-motion-hooks t))
    (sldb-beginning-of-backtrace)
    ;;(while (get-text-property (point) 'frame)
    ;;  (sldb-show-frame-details)
    ;;  (sldb-forward-frame))

    (dotimes (i 5)
      (when (get-text-property (point) 'frame)
	(sldb-show-frame-details)
	(sldb-forward-frame)))))

(defvar slime--highlighted nil)

(defun slime-star--highlight-sexp (&optional start end)
  "Highlight the first sexp after point."
  (slime--delete-highlights)
  (let* ((start (or start (point)))
	 (end (or end (save-excursion (ignore-errors (forward-sexp)) (point))))
	 (overlay (make-overlay start end)))
    (overlay-put overlay 'face 'secondary-selection)
    (setq slime--highlighted overlay)))

(defun slime--delete-highlights ()
  (when slime--highlighted
    (delete-overlay slime--highlighted)
    (setq slime--highlighted nil)))

(defun slime-star--install-stepper-highlighter ()
  "Use custom highlighter when stepping."
  (advice-add 'slime-highlight-sexp :override 'slime-star--highlight-sexp)
  (advice-add 'sldb-quit :after 'slime--delete-highlights)
  (advice-add 'sldb-abort :after 'slime--delete-highlights)
  (advice-add 'slime-display-eval-result :after (lambda (x) (slime--delete-highlights))))

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
  (:slime-dependencies quicklisp-systems quicksearch slime-help system-browser slime-breakpoints)
  (:swank-dependencies slime-star)
  (:on-load
   ;; setup key bindings
   (slime-star--setup-key-bindings)
   ;; add submenu to SLIME menu
   (slime-star--setup-menus)
   (add-hook 'slime-connected-hook
	     (lambda ()
	       (when slime-star-use-toolbars
		 (slime-toolbars--setup-tool-bar))
	       (when slime-star-use-custom-stepper-highlighter
		 (slime-star--install-stepper-highlighter))))
   (advice-add 'slime-load-contribs :before #'slime-star--add-swank-path)
   ))

(provide 'slime-star)
