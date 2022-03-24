(require 'slime)

(defun slime-room ()
  (interactive)
  (let ((room-str
	 (slime-eval `(cl:with-output-to-string (cl:*standard-output*)
						(cl:room)))))
    (let ((buffer (get-buffer-create "*slime-room*")))
      (with-current-buffer buffer
	(insert room-str)
	(display-buffer buffer)))))

(defun slime-star-setup-key-bindings ()
  )

(defun slime-star--setup-menus ()
  (easy-menu-add-item 'menubar-slime nil
		      '("Tools"
			["System Browser" lisp-system-browser]
			["Quicklisp Systems" quicklisp-systems]
			["Quicksearch" quicksearch])
		      "Documentation"))

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
   ;;(slime-help--add-menu-to-slime)
   (slime-star--setup-menus)))

(provide 'slime-star)
