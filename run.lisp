
(in-package :cl-user)

(require :cl-ci)

(setf hunchentoot:*show-lisp-errors-p* t)
(setf hunchentoot:*catch-errors-p* t)
(hunchentoot:start (make-instance 'hunchentoot:acceptor
				  :port 9081))
