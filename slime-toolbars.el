(require 'slime)

(push (concat (file-name-directory load-file-name) "/images") image-load-path)

(defvar slime-tool-bar-map)

(defun slime-toolbars--init-tool-bar-map ()
  (setf slime-tool-bar-map
	(let ((map (make-sparse-keymap)))
	  (tool-bar-local-item-from-menu 'slime-compile-defun "compile" map slime-mode-map
					 :label "Compile"
					 :vert-only t)
	  (tool-bar-local-item-from-menu 'slime-eval-last-expression "evaluate" map slime-mode-map
					 :rtl "break"
					 :label "Evaluate"
					 :vert-only t)
	  (tool-bar-local-item-from-menu 'slime-inspect "inspect" map slime-mode-map
					 :rtl "inspect"
					 :label "Inspect"
					 :vert-only t)
	  (tool-bar-local-item-from-menu 'slime-edit-definition "jump-to" map slime-mode-map)
	  (define-key-after map [separator-1] menu-bar-separator)
	  (tool-bar-local-item-from-menu 'slime-break-on-entry "break" map slime-mode-map)
	  (tool-bar-local-item-from-menu 'slime-step-on-entry "run" map slime-mode-map)
	  (tool-bar-local-item-from-menu 'slime-trace "watch" map slime-mode-map)
	  (define-key-after map [separator-2] menu-bar-separator)
	  (tool-bar-local-item-from-menu 'slime-help-symbol "help" map slime-help-mode-map)
	  (tool-bar-local-item-from-menu 'lisp-system-browser "system-browser" map slime-mode-map)
	  (tool-bar-local-item-from-menu 'slime-load-file "load-file" map slime-mode-map)
	  (tool-bar-local-item-from-menu 'slime-load-system "load-system" map slime-mode-map)  
	  map)))

(defvar sldb-tool-bar-map)

(defun slime-toolbars--init-sldb-tool-bar-map ()
  (setf sldb-tool-bar-map
	(let ((map (make-sparse-keymap)))
	  (tool-bar-local-item-from-menu 'sldb-step "step" map sldb-mode-map
					 :label "Step in"
					 :vert-only t)
	  (tool-bar-local-item-from-menu 'sldb-next "next" map sldb-mode-map
					 :label "Step next"
					 :vert-only t)
	  (tool-bar-local-item-from-menu 'sldb-out "finish" map sldb-mode-map
					 :label "Step out"
					 :vert-only t)
	  map)))

(defun slime-toolbars--setup-tool-bar ()
  (slime-toolbars--init-tool-bar-map)
  (slime-toolbars--init-sldb-tool-bar-map)
  (add-hook 'slime-mode-hook
	    (lambda ()
              (setq-local tool-bar-map slime-tool-bar-map)))
  (add-hook 'slime-repl-mode-hook
	    (lambda ()
              (setq-local tool-bar-map slime-tool-bar-map)))
  (add-hook 'sldb-mode-hook
	    (lambda ()
	      (setq-local tool-bar-map sldb-tool-bar-map))))

(provide 'slime-toolbars)
