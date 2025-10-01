(defpackage :slime-star/presentation-menus
  (:use :cl))

(in-package :slime-star/presentation-menus)

;; hook after an asdf system is loaded
(defmethod asdf:operate :after ((operation asdf:load-op) component &key)
  (break "Loaded: ~s" component))
