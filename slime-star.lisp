(require :def-properties (merge-pathnames "cl-def-properties/module.lisp" *load-pathname*))

(push (merge-pathnames #p"slime-doc-contribs/" *load-pathname*) swank::*load-path*)
(push (merge-pathnames #p"slime-breakpoints/" *load-pathname*) swank::*load-path*)
(push (merge-pathnames #p"quicklisp-systems/" *load-pathname*) swank::*load-path*)
(push (merge-pathnames #p"lisp-system-browser/" *load-pathname*) swank::*load-path*)
(push (merge-pathnames #p"slite/" *load-pathname*) swank::*load-path*)

(provide :slime-star)
