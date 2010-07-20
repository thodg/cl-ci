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

(defclass repository ()
  ((name :type string
	 :initarg :name)
   (dir :type pathname
	:initarg :dir)))

(defvar *repository-types*
  (make-hash-table :test 'equal)
  "Hash table mapping each repo-type string to a repository class name")

(defun make-repository (type name dir &rest initargs)
  (let ((class (gethash type *repository-types*)))
    (unless class
      (error "Unknown repository type ~S" type))
    (apply 'make-instance class :name name :dir dir initargs)))

(defgeneric repository.export (repository target-dir &optional revision))

(defun repository.run-tests (repository
			     &key
			     revision
			     test-dir
			     (log-stream *standard-output*))
  "
Run tests for specific revision of a repository in TEST-DIR.
If TEST-DIR is NIL, the given revision of repository is exported to a
temporary directory in which tests are run.
If LOG-STREAM is given it must be a stream the test log will be written to.
The function returns T when tests passed without exception raised, or NIL.
"
  (declare (type repository repository))
  (with-slots (name) repository
    (if test-dir
	(let ((asdf:*central-registry* asdf:*central-registry*))
	  (pushnew test-dir asdf:*central-registry*)
	  (asdf::with-output-translation (asdf:disable-output-translations)
	    (let ((asdf:*asdf-verbose* t)
		  (*standard-output* log-stream))
	      (catch 'test-result
		(handler-bind ((t (lambda (e)
				    (format log-stream "~&~%~A~%" e)
				    (trivial-backtrace:print-backtrace
				     e :output log-stream)
				    (throw 'test-result (values nil e)))))
		  (asdf:operate 'asdf:test-op name)
		  t)))))
	(with-temporary-dir (tmp-dir (format nil "~A.run-tests" name) t)
	  (repository.export repository tmp-dir revision)
	  (format t "~&Running tests for ~A rev ~A in ~A~%"
		  name revision tmp-dir)
	  (repository.run-tests repository
				:revision revision
				:test-dir tmp-dir
				:log-stream log-stream)))))

(in-package :asdf)

(defmacro with-output-translation (config &body body)
  (let ((previous-translations (gensym "PREV-")))
    `(let ((,previous-translations (output-translations)))
       (unwind-protect
	    (progn
	      ,(cond ((eq (symbol-package (car config)) (find-package :symbol))
		      `(initialize-output-translations ,config))
		     ((consp config)
		      config)
		     (t
		      `(initialize-output-translations '(,config))))
	      ,@body)
	 (setf (output-translations) ,previous-translations)))))
