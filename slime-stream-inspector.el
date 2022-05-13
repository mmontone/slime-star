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

(defun slime-inspect-printed ()
  "Inspect printed object at point."
  (interactive)
  (cl-block nil
    ;; First try with persistent pointers:
    (let ((obj-persistent-pointer (sti--parse-persistent-object-pointer)))
      (when obj-persistent-pointer
	(slime-inspect (format "(stream-inspector:get-lisp-obj-by-persistent-pointer %d)" obj-persistent-pointer))
	(cl-return)))
    ;; If couldn't find persistent pointer, try with object address:
    (let ((obj-address-str (sti--parse-sbcl-address)))
      (when obj-address-str
	(let ((obj-address (string-to-number obj-address-str 16)))
          (when obj-address
            (slime-inspect (format "(stream-inspector:get-lisp-obj-by-address %d)" obj-address))))))))

(defun sti--parse-persistent-object-pointer ()
  "Try to parse object persistent pointer."
  (let (from to)
    (save-excursion
      (search-backward "#<")
      (setf to (point))
      (search-backward "#")
      (setf from (point))
      (when (and from to)
	(let ((substr (buffer-substring-no-properties (1+ from) to)))
	  (when (every (lambda (char)
			 (<= ?0 char ?9))
		       substr)
	    (let ((pointer (string-to-number substr)))
	      (when (not (zerop pointer))
		pointer))))))))

(defun slime-send-printed-to-repl ()
  "Send printed object to SLIME repl."
  (interactive)
  (let ((obj-address-str (sti--parse-sbcl-address)))
    (when obj-address-str
      (let ((obj-address (string-to-number obj-address-str 16)))
        (when obj-address
          (slime-repl-send-string (format "(stream-inspector:get-lisp-obj-by-address #x%s)" obj-address-str)))))))

(defun sti--parse-sbcl-address ()
  "Parse an SBCL readable object address.

Example: For #<CLOG-GUI:CLOG-GUI-WINDOW {1004611BC3}>, it returns \"1004611BC3\""
  (let (from to)
    (save-excursion
      (search-backward "#<")
      (search-forward "{")
      (setf from (point))
      (search-forward "}")
      (setf to (point))
      (buffer-substring-no-properties from (1- to)))))

(defun slime-stream-inspector-init ())

(define-slime-contrib slime-stream-inspector
  "SLIME extension for inspecting objects printed in output streams."
  (:authors "Mariano Montone")
  (:license "GPL")
  (:swank-dependencies stream-inspector))

(provide 'slime-stream-inspector)

;;; slime-stream-inspector.el ends here
