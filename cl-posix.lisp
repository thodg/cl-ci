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

(defmacro sh (format-string &rest format-args)
  `(progn
     (when (boundp 'hunchentoot:*acceptor*)
       (hunchentoot:log-message :info ,format-string ,@format-args))
     (trivial-shell:shell-command
      (let ((*print-case* :downcase))
	(format nil ,format-string ,@format-args)))))

(defun sh-quote (value)
  (concatenate 'string
	       "\""
	       (cl-ppcre:regex-replace-all "([\\'\"])"
					   (format nil "~A" value)
					   "\\\\\\1")
	       "\""))

#+sbcl
(defun make-temporary-dir (prefix &optional (pathname-defaults
					     *default-pathname-defaults*))
  (pathname
   (concatenate 'string
		(sb-posix:mkdtemp (format nil "~A-XXXXXX"
					  (merge-pathnames prefix
							   pathname-defaults)))
		"/")))

#+sbcl
(defun remove-directory (pathname)
  (restart-case (sb-posix:rmdir pathname)
    (retry ()
      :report (lambda (s)
		(format s "Retry removing directory ~S" pathname))
      (remove-directory pathname))
    (continue ()
      :report (lambda (s)
		(format s "Continue without removing directory ~S"
			pathname)))
    (force-remove ()
      :report (lambda (s)
		(format s "Force removing all the contents of directory ~S"
			pathname))
      ;; TODO: implement rm -rf without shell
      (sh "rm -rf ~A" (sh-quote pathname)))))

(defmacro with-temporary-dir ((var &optional (name "tmp")) &body body)
  (let ((tmpdir (gensym "TMPDIR-")))
    `(let ((,tmpdir (make-temporary-dir ,name *temporary-directory*)))
       (unwind-protect (let ((,var ,tmpdir))
			 ,@body)
	 (remove-directory ,tmpdir)))))
