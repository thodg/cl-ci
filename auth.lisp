;;
;;  CL-CI  -  Continuous integration server in Common Lisp
;;
;;  Copyright 2010 Thomas de Grivel <billitch@gmail.com>
;;
;;  Permission is hereby granted, free of charge, to any person
;;  obtaining a copy of this software and associated documentation
;;  files (the "Software"), to deal in the Software without
;;  restriction, including without limitation the rights to use, copy,
;;  modify, merge, publish, distribute, sublicense, and/or sell copies
;;  of the Software, and to permit persons to whom the Software is
;;  furnished to do so, subject to the following conditions:
;;  
;;  The above copyright notice and this permission notice shall be
;;  included in all copies or substantial portions of the Software.
;;
;;  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;  EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;  MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;  NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;;  HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
;;  WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;;  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;;  DEALINGS IN THE SOFTWARE.
;;

(in-package :cl-ci)

(defun read-auth (&optional (auth-file *auth-file*))
  (let ((auth (make-hash-table :test 'equal)))
    (with-open-file (stream auth-file)
      (let ((*read-eval* nil))
	(loop
	   for exp = (read stream nil :eof)
	   while (not (eq :eof exp))
	   do (destructuring-bind (login pass &rest plist) exp
		(setf (gethash (cons login pass) auth) plist)))))
    auth))

(defun write-auth (auth &optional (auth-file *auth-file*))
  (with-open-file (stream auth-file
			  :direction :output
			  :if-exists :rename)
    (format stream ";;  -*- lisp -*-
;;
;;  Config file for CL-CI authentification
;;
;;  Each top-level form must match the lambda-list :
;;  (login-string pass-string &rest plist)
;;
;;  Everything after :eof is ignored.
;;

")
    (maphash #'(lambda (key plist)
		 (let ((login (car key))
		       (pass (cdr key)))
		   (write (list* login pass plist)
			  :stream stream
			  :readably t
			  :pretty t
			  :case :downcase
			  :right-margin 70)))
	     auth)
    (format stream "~&~%:eof~%")))

(defun auth-add (login pass &rest plist)
  (let ((auth (read-auth)))
    (setf (gethash (cons login pass) auth) plist)
    (write-auth auth)))

(defun auth-check (login pass &key kind)
  (let ((auth (read-auth)))
    (multiple-value-bind (plist present) (gethash (cons login pass)
						  auth)
      (unless present
	(return-http 401))
      (when kind
	(unless (eq kind (getf plist :kind))
	  (return-http 403))))))
