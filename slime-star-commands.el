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

(defun sldb-kill-all-buffers ()
  "Kill all SLDB buffers."
  (interactive)
  (dolist (buf (sldb-buffers))
    (kill-buffer buf)))

(defun sldb-show-all-frames-details ()
  "Show details of all frames."
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

(provide 'slime-star-commands)

;;; slime-star-commands.el ends here
