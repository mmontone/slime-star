;; Emacs init file

(when (>= emacs-major-version 24)
  (require 'package)
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
  (package-initialize))

(require 'compat)

;; Setup load-path, autoloads and your lisp system
(add-to-list 'load-path "~/.emacs.d/slime-star")

;; Add slime-star to slime-contribs:
(setq slime-contribs '(slime-fancy slime-star))
(setq inferior-lisp-program "sbcl")

(slime)

(find-file "~/.emacs.d/slime-star/README.org")
(display-buffer "README.org")
