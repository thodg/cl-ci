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

(defmacro define-hook (name parameters &body body)
  `(hunchentoot:define-easy-handler (,(intern (format nil "DISPATCH-~A" name))
				      :uri ,(format nil "/hook/~A"
						    (string-downcase name)))
       ,parameters
     ,@body))

(defclass report ()
  ((event :type string :initarg :event)
   (repository :type repository :initarg :repository)
   (revision :type string :initarg :revision)
   (try :type integer)
   (uri :type string)
   (path :type pathname)
   (stream :type stream)
   (result :initform nil)))

(defun make-report (event repository revision)
  (let ((repo-name (slot-value repository 'name))
	(report (make-instance 'report
			       :event event
			       :repository repository
			       :revision revision)))
    (ensure-directories-exist (format nil "files/reports/~A/" repo-name))
    (loop
       for try% from 1
       for uri% = (format nil "/reports/~A/~A.~3,'0d.txt"
			  repo-name revision try%)
       for path% = (format nil "files~A" uri%)
       for stream% = (open path%
			   :direction :output
			   :if-exists nil
			   :if-does-not-exist :create)
       until stream%
       finally (with-slots (try uri path stream) report
		 (setf try try%
		       uri uri%
		       path path%
		       stream stream%)))
    report))

(defmacro with-report (stream-var (event repository revision) &body body)
  (let ((report (gensym "REPORT-"))
	(stream (gensym "STREAM-"))
	(result (gensym "RESULT-")))
    `(let* ((,report (make-report ,event ,repository ,revision))
	    (,stream (slot-value ,report 'stream)))
       (unwind-protect (let* ((,stream-var ,stream)
			      (,result (progn ,@body)))
			 (format ,stream "~&~%Result : ~S~%" ,result)
			 (setf (slot-value ,report 'result) ,result)
			 ,report)
	 (close ,stream)))))

(defun notify (report)
  (with-slots (event) report
    (flet ((notify-action (action)
	     (apply (case (first action)
		      (:email 'notify-email)
		      (otherwise (hunchentoot:log-message
				  :info "Unknown notify action : ~S"
				  action)
				 'notify-unknown))
		    report
		    (rest action))))
      (let ((conf (read-notify)))
	(mapcar #'notify-action (getf conf event))
	(mapcar #'notify-action (getf conf t))))))

(define-hook update (repository key old-rev new-rev repo-type repo-dir)
  (check-type repository string)
  (check-type key string)
  (check-type old-rev string)
  (check-type new-rev string)
  (check-type repo-type string)
  (check-type repo-dir string)
  (auth-check repository key :kind :repository)
  (let* ((repo (make-repository repo-type repository repo-dir))
	 (report
	  (with-report report (:update repo new-rev)
	    (format report "
Checking update of repository ~S
  From revision ~S
  To   revision ~S
  Repo type ~A
  Repo dir ~A

"
		    repository old-rev new-rev repo-type repo-dir)
	    (repository.run-tests repo
				  :revision new-rev
				  :log-stream report))))
    (notify report)
    (hunchentoot:redirect (slot-value report 'uri))))

(defun report-title (report)
  (with-slots (event repository revision try) report
    (with-slots (name) repository
      (format nil "~A of ~A to revision ~A (try ~3,'0d)"
	      (string-capitalize (symbol-name event))
	      name revision try))))

(defun report-text (report)
  (with-slots (path) report
    (with-open-file (stream path)
      (let ((string (make-string (file-length stream))))
	(read-sequence string stream)
	string))))

(defun notify-email (report to)
  (hunchentoot:log-message :info "Sending mail to ~S" to)
  (cl-smtp:send-email *smtp-host* *mail-from* to
		      (report-title report)
		      (report-text report)))

(defun notify-unknown (report &rest args)
  (declare (ignore report args)))
