(defpackage :tag-scraper (:use :cl))
(in-package :tag-scraper)

;; needs drakma, closure-html, css-selectors

(defun http-request (uri &rest args)
  "A wrapper around DRAKMA:HTTP-REQUEST which converts octet array
which it sometimes returns to normal string"
  (let* ((result-mv (multiple-value-list (apply #'drakma:http-request uri `(,@args))))
         (result (car result-mv)))
    (apply #'values
           (if (and (arrayp result)
                    (equal (array-element-type result) '(unsigned-byte 8)))
               (flexi-streams:octets-to-string result)
               result)
           (cdr result-mv))))

(defun parse-url (url &rest args)
  "Parse HTTP request response to CXML-DOM"
  (let ((response (apply #'http-request url args)))
    (chtml:parse response (cxml-dom:make-dom-builder))))

(defun node-text (node &rest args &key test)
  (let (values result)
    (when (or (not test) (funcall test node))
      (dom:do-node-list (node (dom:child-nodes node))
        (let ((val (case (dom:node-type node)
                     (:element (apply #'node-text node args))
                     (:text (dom:node-value node)))))
          (push val values))))
    (setf result (apply #'concatenate 'string (nreverse values)))
    result))


(defun tag-attributes (tag)
  (mapcar #'node-text
        (let ((document (parse-url (format nil "http://www.w3schools.com/tags/tag_~a.asp"
                                           tag))))
          (css:query "table.reference tbody tr td a" document))))


(defun define-tag-form (tag-name)
  `(whopper:def-html-tag ,(intern (string-upcase tag-name) :whopper-tags)
       ,@(mapcar #'(lambda (attr) (intern (string-upcase attr) :whopper))
                 (tag-attributes tag-name))))
