;;; slime-toolbars.el --- Toolbars for SLIME

;;; Commentary:
;; 

(require 'slime)

;;; Code:

(push (concat (file-name-directory load-file-name) "/images") image-load-path)

(defcustom slime-toolbars-slime-tool-bar-spec
  '((slime-compile-defun :help "Compile expression")
    (slime-eval-last-expression :help "Evaluate expression")
    (slime-inspect :help "Inspect expression")
    (slime-edit-definition :help "Goto definition") |
    (slime-break-on-entry :help "Break on entry")
    (slime-step-on-entry :help "Step on entry")
    (slime-trace :help "Trace function")
    (slime-help-symbol :map slime-help-mode-map
		       :help "Definition documentation") |
    (lisp-system-browser :help "Open Lisp browser")
    (slime-load-file :help "Load Lisp file")
    (slime-compile-file :help "Compile Lisp file")
    (slime-load-system :help "Load ASDF system"))
  "SLIME toolbar spec."
  :type '(set symbol)
  :group 'slime-star)

(defcustom slime-toolbars-sldb-tool-bar-spec
  '((sldb-down :help "Go to frame down")
    (sldb-up :help "Go to frame up") |
    (sldb-toggle-details :help "Toggle frame details")
    (sldb-show-source :help "Go to frame source")
    (sldb-eval-in-frame :help "Evaluate in frame")
    (sldb-inspect-in-frame :help "Inspect in frame") |
    (sldb-step :help "Step in")
    (sldb-next :help "Step next")
    (sldb-out :help "Step out")
    (sldb-quit :help "Quit debugger"))
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
	  ;; See 23.18.1.2 Extended Menu Items for the list of available
	  ;; properties in tool-bar-local-item
	  ;; See 23.18.6 Tool bars
	  (tool-bar-local-item icon command command map
			       :visible t
			       :keys ""
			       :help (and (listp item)
					  (cl-getf (rest item) :help))))))
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

(provide 'slime-toolbars)

;;; slime-toolbars.el ends here
