(require 'slime)

(push (concat (file-name-directory load-file-name) "/images") image-load-path)

(defcustom slime-toolbars-tool-bar-spec
  '(slime-compile-defun
    slime-eval-last-expression
    slime-inspect
    slime-edit-definition |
    slime-break-on-entry
    slime-step-on-entry
    slime-trace
    (slime-help-symbol :map slime-help-mode-map) |
    lisp-system-browser
    slime-load-file
    slime-compile-file
    slime-load-system)
  "SLIME toolbar spec."
  :type '(set symbol)
  :group 'slime-star)

(defvar slime-tool-bar-icons
  '((slime-compile-defun . "compile")
    (slime-eval-last-expression . "evaluate")
    (slime-inspect . "inspect")
    (slime-edit-definition . "jump-to")
    (slime-break-on-entry . "break")
    (slime-step-on-entry . "run")
    (slime-trace . "watch")
    (slime-help-symbol . "help")
    (lisp-system-browser . "system-browser")
    (slime-compile-file . "compile-file")
    (slime-load-file . "load-file")
    (slime-load-system . "load-system")))

(defun create-tool-bar-map-from-spec (spec default-map icons)
  "Create a tool-bar map from SPEC.
DEFAULT-MAP is the default key-map to use when it is not explicitely specified in SPEC.
ICONS should be an association list with (COMMAND . ICON-NAME)."
  (let ((map (make-sparse-keymap)))
    (dolist (item spec)
      (let* ((command (cond
		     ((symbolp item)
		      item)
		     ((listp item)
		      (first item))
		     (t (error "Invalid item: %s" item))))
	    (separator-p (eql item '|))
	    (from-map (or (and (listp item)
			       (let ((map-symbol (cl-getf (rest item) :map)))
				 (and map-symbol (eval map-symbol))))
			  default-map))
	    (icon (or (and (listp item)
			   (cl-getf (rest item) :icon))
		      (or separator-p (alist-get command icons))
		      (error "No icon for: %s" command))))
	(if separator-p
	    (define-key-after map (make-vector 1 (gensym)) menu-bar-separator)
	  (tool-bar-local-item-from-menu command icon map from-map))))
    map))

(defvar slime-tool-bar-map)

(defun slime-toolbars--init-tool-bar-map ()
  (setf slime-tool-bar-map
	(create-tool-bar-map-from-spec slime-toolbars-tool-bar-spec
				       slime-mode-map slime-tool-bar-icons)))

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
