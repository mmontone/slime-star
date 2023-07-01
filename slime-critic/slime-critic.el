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

;; A problem with compilation-mode is that it uses line and column for positions,
;; but critiques contain raw file position.
;; So we need to convert opening a buffer and line-number-at-pos

(defun slime-critic--convert-file-positions-to-line (critiques file)
  "Convert file positions in CRITIQUES for FILE."
  (let ((buffer (find-file-noselect file)))
    (with-current-buffer buffer
      (mapcar (lambda (critique)
		(let ((line (line-number-at-pos (1+ (car critique)))))
		  (cons line (cdr critique))))
	      critiques))))

(defun slime-critic--show-critiques (critiques file)
  "Show the Lisp CRITIQUES."
  (let ((buffer (get-buffer-create "*slime-critic*"))
	(critiques (slime-critic--convert-file-positions-to-line critiques file)))
    (with-current-buffer buffer
      (setq buffer-read-only nil)
      (erase-buffer)
      (if (null critiques)
	  (insert "Nothing to point out. Well done!")
	(dolist (critique critiques)
	  (insert (format "%s:%d:%d" file (car critique) 0))
	  (newline)
          (insert (cdr critique))
          (newline 2)))
      (compilation-mode)
      (setq buffer-read-only t)
      (display-buffer buffer))))

(defun slime-critic-critique-file (file)
  "Critique the FILE."
  (interactive "fCritique file: ")
  (slime-eval-async `(slime-critic:critique-file ,file)
    (lambda (critiques)
      (slime-critic--show-critiques critiques file))))

(defun slime-critic--create-note (critique buffer)
  "Create a slime-note from CRITIQUE."
  ;; See slime-goto-source-location for location format
  (list :severity :style-warning
        :message (cdr critique)
        :location (list :location
			(list :buffer buffer)
			(list :position (1+ (car critique)))
			nil)
        :source-context nil))

(defun slime-critic-critique-buffer ()
  "Critique current buffer."
  (interactive)
  (unless buffer-file-name
    (user-error "No file visited in current buffer"))
  (let ((buffer (current-buffer)))
    (slime-eval-async `(slime-critic:critique-file ,buffer-file-name)
      (lambda (critiques)
        (slime-critic--show-critiques critiques buffer-file-name)
        (let ((notes (mapcar (lambda (c)
			       (slime-critic--create-note c buffer))
			     critiques)))
          (with-current-buffer buffer
            (slime-highlight-notes notes)))))))

(defun slime-critic--add-to-slime-menu ()
  "Add slime-critic menu to slime menu."
  (easy-menu-add-item 'menubar-slime nil
		      '("Critic"
			["Critique file" slime-critic-critique-file
			 :help "Critique a file"]
			["Critique buffer" slime-critic-critique-buffer
			 :help "Critique a buffer"])))

(define-slime-contrib slime-critic
  "SLIME extension for lisp-critic"
  (:authors "Mariano Montone")
  (:license "GPL")
  (:swank-dependencies slime-critic)
  (:on-load
   ;; add submenu to SLIME menu
   (slime-critic--add-to-slime-menu)))

(provide 'slime-critic)

;;; slime-critic.el ends here
