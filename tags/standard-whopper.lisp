;; -*- lisp -*-

(in-package :whopper)

;;;; * Standard, non HTML, WHOPPER tags

(deftag <:progn (&body body)
  (emit-body body))

(defun <:format (message &rest args)
  (write-string (escape-as-html (apply #'format nil message args))
                *whopper-stream*)
  (values))

(deftag <:as-html (&body text)
  (dolist (txt text)
    (emit-html txt)))

(deftag-macro <:ah (&body text)
  `(<:as-html ,@text))

(deftag-macro <:ai (&attribute quotedp &body text)
  `(<:as-is :quotedp ,quotedp ,@text))

(deftag <:as-is (&attribute quotedp &body text)
  (when quotedp
    (emit-princ ~% "<![CDATA["))
  (dolist (txt text)
    (emit-princ txt))
  (when quotedp
    (emit-princ "]]>" ~%)))

(deftag <:call-with-whopper-stream (stream-var &body body)
  (emit-code `(let ((,stream-var *whopper-stream*)) ,@body)))
