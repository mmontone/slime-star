;;; slime-toolbars.el --- Toolbars for SLIME

;;; Commentary:
;;

(require 'slime)

;;; Code:

(push (concat (file-name-directory load-file-name) "/images") image-load-path)

(defcustom slime-toolbars-slime-tool-bar-spec
  '((slime-compile-defun :help "Compile expression"
                         :icon "compile")
    (slime-eval-last-expression :help "Evaluate expression"
                                :icon "evaluate")
    (slime-inspect :help "Inspect expression"
                   :icon "inspect")
    (slime-edit-definition :help "Goto definition"
                           :icon "jump-to")
    |
    (slime-break-on-entry :help "Break on entry"
                          :icon "break")
    (slime-step-on-entry :help "Step on entry"
                         :icon "run")
    (slime-toggle-fancy-trace :help "Trace function"
                              :icon "watch")
    (slime-help-symbol :map slime-help-mode-map
                       :help "Definition documentation"
                       :icon "help")
    |
    (lisp-system-browser :help "Open Lisp browser"
                         :icon "system-browser")
    (slime-load-file :help "Load Lisp file"
                     :icon "load-file")
    (slime-compile-file :help "Compile Lisp file"
                        :icon "compile-file")
    (slime-load-system :help "Load ASDF system"
                       :icon "load-system"))
  "SLIME toolbar spec."
  :type '(set symbol)
  :group 'slime-star)

(defcustom slime-toolbars-sldb-tool-bar-spec
  '((sldb-down :help "Go to frame down"
               :icon "down")
    (sldb-up :help "Go to frame up"
             :icon "up")
    |
    (sldb-toggle-details :help "Toggle frame details"
                         :icon "all")
    (sldb-show-source :help "Go to frame source"
                      :icon "jump-to")
    (sldb-eval-in-frame :help "Evaluate in frame"
                        :icon "evaluate")
    (sldb-inspect-in-frame :help "Inspect in frame"
                           :icon "inspect")
    |
    (sldb-step :help "Step in"
               :icon "step")
    (sldb-next :help "Step next"
               :icon "next")
    (sldb-out :help "Step out"
              :icon "finish")
    (sldb-quit :help "Quit debugger"
               :icon "stop"))
  "SLDB toolbar spec."
  :type '(set symbol)
  :group 'slime-star)

(defun create-tool-bar-map-from-spec (spec &optional default-map &rest defaults)
  "Create a tool-bar map from SPEC.
DEFAULT-MAP is the default key-map to use when it is not explicitely specified in SPEC.
DEFAULTS can contain certain defaults, like :vert-only."
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
                       (and (listp item)
                            (cl-getf (rest item) :icon))))
             (help (and (listp item)
                        (member :help (rest item))
                        (cl-getf (rest item) :help)))
             (vert-only (or
                         (and (listp item)
                              (member :vert-only (rest item))
                              (cl-getf (rest item) :vert-only))
                         (cl-getf defaults :vert-only))))
        (if separator-p
            (define-key-after map (make-vector 1 (gensym)) menu-bar-separator)
          ;; See 23.18.1.2 Extended Menu Items for the list of available
          ;; properties in tool-bar-local-item
          ;; See 23.18.6 Tool bars
          (if from-map
              (tool-bar-local-item-from-menu command icon map from-map
                                             :help help
                                             :vert-only vert-only)
            (tool-bar-local-item icon command command map
                                 :vert-only vert-only
                                 :help help)))))
    map))

(defvar slime-tool-bar-map)

(defun slime-toolbars--init-slime-tool-bar-map ()
  (setq slime-tool-bar-map
        (create-tool-bar-map-from-spec slime-toolbars-slime-tool-bar-spec nil :vert-only t)))

(defvar sldb-tool-bar-map)

(defun slime-toolbars--init-sldb-tool-bar-map ()
  (setq sldb-tool-bar-map
        (create-tool-bar-map-from-spec slime-toolbars-sldb-tool-bar-spec nil :vert-only t)))

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
