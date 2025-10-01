(in-package :slime-star/presentation-menus)

(defmethod swank::menu-choices-for-presentation ((object hunchentoot:acceptor))
  (list
   (list "Start" (lambda (choice acceptor id)
                   (declare (ignore choice id))
                   (hunchentoot:start acceptor)
                   nil))
   (list "Stop" (lambda (choice acceptor id)
                  (declare (ignore choice id))
                  (hunchentoot:stop acceptor)
                  nil))))
