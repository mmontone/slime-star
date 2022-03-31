(require 'slime)

(push (concat (file-name-directory load-file-name) "/images") image-load-path)

(defcustom slime-toolbars-slime-tool-bar-spec
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

(defcustom slime-toolbars-sldb-tool-bar-spec
  '(
    sldb-down
    sldb-up |
    sldb-toggle-details
    sldb-show-source
    sldb-eval-in-frame
    sldb-inspect-in-frame |
    sldb-step
    sldb-next
    sldb-out
    sldb-quit
    )
  "SLDB toolbar spec."
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

(defvar sldb-tool-bar-icons
  '((sldb-step . "step")
    (sldb-next . "next")
    (sldb-out . "finish")
    (sldb-eval-in-frame . "evaluate")
    (sldb-inspect-in-frame . "inspect")
    (sldb-down . "down")
    (sldb-up . "up")
    (sldb-show-source . "jump-to")
    (sldb-toggle-details . "all")
    (sldb-quit . "stop")))

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
             (icon (or separator-p
		       (or
			(and (listp item)
                             (cl-getf (rest item) :icon))
			(alist-get command icons)
			(error "No icon for: %s" command)))))
        (if separator-p
            (define-key-after map (make-vector 1 (gensym)) menu-bar-separator)
	  (tool-bar-local-item icon command command map))))
    map))

(defvar slime-tool-bar-map)

(defun slime-toolbars--init-slime-tool-bar-map ()
  (setq slime-tool-bar-map
        (create-tool-bar-map-from-spec slime-toolbars-slime-tool-bar-spec
                                       slime-mode-map slime-tool-bar-icons)))

(defvar sldb-tool-bar-map)

(defun slime-toolbars--init-sldb-tool-bar-map ()
  (setq sldb-tool-bar-map
        (create-tool-bar-map-from-spec slime-toolbars-sldb-tool-bar-spec
                                       sldb-mode-map
                                       sldb-tool-bar-icons)))

(defun slime-toolbars-setup-tool-bars ()
  (slime-toolbars--init-slime-tool-bar-map)
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
