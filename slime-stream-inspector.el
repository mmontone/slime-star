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
  (let ((obj-address-str (sti--parse-sbcl-address)))
    (when obj-address-str
      (let ((obj-address (string-to-number obj-address-str 16)))
        (when obj-address
          (slime-inspect (format "(stream-inspector:get-lisp-obj-by-address %d)" obj-address)))))))

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
