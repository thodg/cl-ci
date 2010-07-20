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

(defun last-path-component (path)
  (if (cl-fad:directory-pathname-p path)
      (namestring (make-pathname
		   :directory (cons :relative (last (pathname-directory path)))))
      (namestring (make-pathname :name (pathname-name path)
                                 :type (pathname-type path)))))

(defun human-readable-size (bytes)
  (cond
    ((< bytes (expt 10 3))
     (format nil "~d B" (truncate bytes)))
    ((< bytes (expt 10 6))
     (format nil "~0,1f kB" (/ bytes (expt 10 3))))
    ((< bytes (expt 10 9))
     (format nil "~0,1f MB" (/ bytes (expt 10 6))))
    ((< bytes (expt 10 12))
     (format nil "~0,1f GB" (/ bytes (expt 10 9))))
    ((< bytes (expt 10 15))
     (format nil "~0,1f TB" (/ bytes (expt 10 12))))
    ((< bytes (expt 10 18))
     (format nil "~0,1f PB" (/ bytes (expt 10 15))))
    (t (format nil "~0,1f EB" (/ bytes (expt 10 18))))))

(defun file-size (path)
  (with-open-file (stream path)
    (file-length stream)))

(defun format-date (utime)
  (multiple-value-bind
	(second minute hour date month year day-of-week dst-p tz)
      (decode-universal-time utime)
    (declare (ignore day-of-week dst-p tz))
    (format nil "~a-~a-~a ~2,'0d:~2,'0d:~2,'0d"
	    date (nth (1- month) hunchentoot-dir-lister::*months*)
	    year hour minute second)))

(defun list-directory-html (dir uri-path
                            &key (show-hidden-files nil)
                            (exclude-prefixes '("." "#"))
                            (exclude-suffixes '("~"))
			    uri-prefix)
                            ;(style *default-style*)
                            ;style-link
                            ;javascript-link
                            ;on-load)
  (let ((uri (format nil "~A~A" uri-prefix uri-path)))
    (render-parts
     uri
     (list #P"file-list.xhtml"
	   :url uri
	   :files (sort
		   (mapcar
		    #'(lambda (path)
			(when (hunchentoot-dir-lister::path-passes-p
			       path uri-path exclude-prefixes
			       exclude-suffixes show-hidden-files)
			  (let ((name (last-path-component path))
				(date (file-write-date path)))
			    (list :name name
				  :url (format nil "~A~A" uri name)
				  :size (human-readable-size
					 (file-size path))
				  :wtime date
				  :date (format-date date)))))
		    (cl-fad:list-directory dir))
		   #'> :key #'(lambda (f)
				(getf f :wtime 0)))))))

(defmacro define-files-dispatchers (dir &key index type)
  (let* ((dir-string (string-downcase (symbol-name dir)))
	 (uri-prefix (format nil "/~A/" dir-string))
	 (base-path (concatenate 'string "files/" dir-string "/"))
	 (dispatch-sym (intern (format nil "DISPATCH-~A" dir))))
    (setf (symbol-function dispatch-sym)
	  (if index
	      (hunchentoot-dir-lister:create-directory-listing-dispatcher
	       uri-prefix base-path 'list-directory-html
	       :uri-prefix (format nil "/~A" dir-string))
	      (hunchentoot:create-folder-dispatcher-and-handler
	       uri-prefix base-path type)))
    `',dispatch-sym))

(define-files-dispatchers css :type "text/css")
(define-files-dispatchers img :type "image/png")
(define-files-dispatchers js  :type "text/javascript")
(define-files-dispatchers reports :index t :type "text/html")

(hunchentoot:define-easy-handler (home :uri "/") ()
  (render-parts nil `(#P"home.xhtml"
			:intro "Welcome to CL-CI")))

(setf hunchentoot:*handle-http-errors-p* t)
(setf hunchentoot:*approved-return-codes* '(200 204 207 304 401 403))

(setf hunchentoot:*dispatch-table*
      `(hunchentoot:dispatch-easy-handlers
	dispatch-css
	dispatch-reports))

(setf hunchentoot:*access-log-pathname* #P"log/access.log")
(setf hunchentoot:*message-log-pathname* #P"log/message.log")
