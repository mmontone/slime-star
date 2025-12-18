;;; inline-message.el --- Show messages inlined in buffers  -*- lexical-binding: t; -*-

;; This is code modified from Cider project
;; https://github.com/clojure-emacs/cider/blob/master/cider-overlays.el

;; Copyright © 2015-2022 Bozhidar Batsov, Artur Malabarba and CIDER contributors

;; Author: Artur Malabarba <bruce.connor.am@gmail.com>
;; Version: 0.2

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

;;; Show messages inlined in buffers.

;;; Code:

(defgroup inline-message nil
  "Buffer inline messages."
  :group 'tools)

(defface inline-message-overlay-face
  '((((class color) (background light))
     :background "grey90" :box (:line-width -1 :color "yellow"))
    (((class color) (background dark))
     :background "grey10" :box (:line-width -1 :color "black")))
  "Face used to display evaluation results at the end of line.
If `inline-message-use-font-lock' is non-nil, this face is
applied with lower priority than the syntax highlighting."
  :group 'inline-message)

(defcustom inline-message-overlay-position 'at-eol
  "Where to display result overlays for inline evaluation and the debugger.
If 'at-eol, display at the end of the line.
If 'at-point, display at the end of the respective sexp."
  :group 'inline-message
  :type ''(choice (const :tag "End of line" at-eol)
                  (const :tag "End of sexp" at-point)))

(defcustom inline-message-use-font-lock t
  "If non-nil, results overlays are font-locked.
If nil, apply `inline-message-overlay-face' to the entire overlay instead of
font-locking it."
  :group 'inline-message
  :type 'boolean)

(defcustom inline-message-duration 'command
  "Duration, in seconds, of inline-message overlays.
If nil, overlays last indefinitely.
If the symbol `command', they're erased after the next command.
If the symbol `change', they last until the next change to the buffer."
  :type '(choice (integer :tag "Duration in seconds")
                 (const :tag "Until next command" command)
                 (const :tag "Until next buffer change" change)
                 (const :tag "Last indefinitely" nil))
  :group 'inline-message)

(defun inline-message--make-overlay (l r type &rest props)
  "Place an overlay between L and R and return it.
TYPE is a symbol put on the overlay's category property.  It is used to
easily remove all overlays from a region with:
    (remove-overlays start end 'category TYPE)
PROPS is a plist of properties and values to add to the overlay."
  (let ((o (make-overlay l (or r l) (current-buffer))))
    (overlay-put o 'category type)
    (overlay-put o 'inline-message-temporary t)
    (while props (overlay-put o (pop props) (pop props)))
    (push #'inline-message--delete-overlay (overlay-get o 'modification-hooks))
    o))

(defun inline-message--delete-overlay (ov &rest _)
  "Safely delete overlay OV.
Never throws errors, and can be used in an overlay's modification-hooks."
  (ignore-errors (delete-overlay ov)))

(defun inline-message--remove-overlay (&rest _)
  "Remove inline message overlay from current buffer.
This function also removes itself from `post-command-hook' and
`after-change-functions'."
  (let ((hook (pcase inline-message-duration
                (`command 'post-command-hook)
                (`change 'after-change-functions))))
    (remove-hook hook #'inline-message--remove-overlay 'local))
  (remove-overlays nil nil 'category 'inline-message))

(defun inline-message--remove-overlay-after-command ()
  "Add `inline-message--remove-overlay' locally to `post-command-hook'.
This function also removes itself from `post-command-hook'."
  (remove-hook 'post-command-hook #'inline-message--remove-overlay-after-command 'local)
  (add-hook 'post-command-hook #'inline-message--remove-overlay nil 'local))

(cl-defun inline-message (message &rest props &key where
                                  (duration 'command)
                                  (type 'inline-message)
                                  (prepend-face 'inline-message-overlay-face) &allow-other-keys)
  "Place an overlay displaying MESSAGE at the position determined by WHERE.
MESSAGE is used as the overlay's after-string property, meaning it is
displayed at the end of the overlay.
Return nil if the overlay was not placed or if it might not be visible, and
return the overlay otherwise.

Return the overlay if it was placed successfully, and nil if it failed.

This function takes some optional keyword arguments:

  If WHERE is a number or a marker, apply the overlay as determined by
  `inline-message-overlay-position'.  If it is a cons cell, the car and cdr
  determine the start and end of the overlay.
  DURATION: Duration, in seconds, of inline-message overlays.
  If nil, overlays last indefinitely.
  If the symbol `command', they're erased after the next command.
  If the symbol `change', they last until the next change to the buffer.

All arguments beyond these (PROPS) are properties to be used on the
overlay."
  (declare (indent 1))
  (while (keywordp (car props))
    (setq props (cdr (cdr props))))
  ;; If the marker points to a dead buffer, don't do anything.
  (let ((buffer (cond
                 ((markerp where) (marker-buffer where))
                 ((markerp (car-safe where)) (marker-buffer (car where)))
                 (t (current-buffer)))))
    (with-current-buffer buffer
      (save-excursion
        (when (number-or-marker-p where)
          (goto-char where))
        ;; Make sure the overlay is actually at the end.
        (when (looking-at "\r\n[:blank:]")
          (skip-chars-backward "\r\n[:blank:]"))
        (let* ((beg (if (consp where)
                        (car where)
                      (save-excursion
                        (ignore-errors (backward-sexp 1))
                        (point))))
               (end (if (consp where)
                        (cdr where)
                      (pcase inline-message-overlay-position
                        ('at-eol (line-end-position))
                        ('at-point (point)))))
               ;; Specify `default' face, otherwise unformatted text will
               ;; inherit the face of the following text.
               (display-string (propertize message 'face 'default))
               (o nil))
          (remove-overlays beg end 'category type)
          (funcall (if inline-message-use-font-lock
                       #'font-lock-prepend-text-property
                     #'put-text-property)
                   0 (length display-string)
                   'face prepend-face
                   display-string)
          ;; If the display spans multiple lines or is very long, display it at
          ;; the beginning of the next line.
          (when (or (string-match "\n." display-string)
                    (> (string-width display-string)
                       (- (window-width) (current-column))))
            (setq display-string (concat " \n" display-string)))
          ;; Put the cursor property only once we're done manipulating the
          ;; string, since we want it to be at the first char.
          (put-text-property 0 1 'cursor 0 display-string)
          (when (> (string-width display-string) (* 3 (window-width)))
            (setq display-string
                  (concat (substring display-string 0 (* 3 (window-width)))
                          "...")))
          ;; Create the message overlay.
          (setq o (apply #'inline-message--make-overlay
                         beg end type
                         'after-string display-string
                         props))
          (pcase duration
            ((pred numberp) (run-at-time duration nil #'inline-message--delete-overlay o))
            (`command
             ;; If inside a command-loop, tell `inline-message--remove-result-overlay'
             ;; to only remove after the *next* command.
             (if this-command
                 (add-hook 'post-command-hook
                           #'inline-message--remove-overlay-after-command
                           nil 'local)
               (inline-message--remove-overlay-after-command)))
            (`change
             (add-hook 'after-change-functions
                       #'inline-message--remove-overlay
                       nil 'local)))
          (when-let* ((win (get-buffer-window buffer)))
            ;; Left edge is visible.
            (when (and (<= (window-start win) (point) (window-end win))
                       ;; Right edge is visible. This is a little conservative
                       ;; if the overlay contains line breaks.
                       (or (< (+ (current-column) (string-width message))
                              (window-width win))
                           (not truncate-lines)))
              o)))))))

(provide 'inline-message)

;;; inline-message.el ends here
