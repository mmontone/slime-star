(in-package :slime-star/presentation-menus)

(defmethod swank::menu-choices-for-presentation ((object sb-thread:thread))
  (list
   (list "Destroy thread"
         (lambda (choice thread id)
           (declare (ignore choice id))
           (bt:destroy-thread thread)
           nil))))
