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

(setf html-template:*template-start-marker* "«")
(setf html-template:*template-end-marker* "»")
(setf html-template:*default-template-pathname* #P"templates/")
(html-template:clear-template-cache)

(defun render-parts (title &rest parts)
  (with-output-to-string (stream)
    (html-template:fill-and-print-template
     #P"layout.xhtml"
     (list :domain *domain*
	   :title title
	   :parts parts)
     :stream stream)))

(defun create-files-dispatchers (&rest items)
  (mapcar #'(lambda (item)
	      (let ((dir (first item))
		    (type (second item)))
		(hunchentoot:create-folder-dispatcher-and-handler
		 (format nil "/~A/" dir)
		 (make-pathname :directory (list "files" dir))
		 type)))
	  items))

(hunchentoot:define-easy-handler (home :uri "/") ()
  (render-parts "Home" `(#P"home.xhtml" ())))

(defmacro define-hook (name parameters &body body)
  `(hunchentoot:define-easy-handler (,(intern (format nil "DISPATCH-~A" name))
				      :uri ,(format nil "/hook/~A"
						    (string-downcase name)))
       ,parameters
     ,@body))

(define-hook update (repository old-rev new-rev repo-type repo-dir)
  (check-type repository string)
  (check-type old-rev string)
  (check-type new-rev string)
  (check-type repo-type string)
  (check-type repo-dir string)
  (hunchentoot:log-message :debug "Repository ~S updated to revision ~S"
			   repository new-rev)
  (format nil "
Checking update of repository ~S
  From rev ~S
  To   rev ~S
  Repo type ~A
  Repo dir ~A
"
	  repository old-rev new-rev repo-type repo-dir))

(setf hunchentoot:*dispatch-table*
      `(hunchentoot:dispatch-easy-handlers
	,@(create-files-dispatchers '("css" "text/css")
				    '("img" "image/png")
				    '("js"  "text/javascript"))))


(setf hunchentoot:*access-log-pathname* #P"log/access.log")
(setf hunchentoot:*message-log-pathname* #P"log/message.log")
