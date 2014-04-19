;; -*- lisp -*-

(in-package :whopper)

;;;; * WHOPPER shortcuts

(defvar *html-prologue* "<!DOCTYPE html
PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\"
\"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\">")

(deftag <:comment (&body body)
  (emit-princ "<!--")
  (emit-body body)
  (emit-princ "-->"))

(deftag-macro <:href (url &allow-other-attributes others &body body)
  (if body
      `(<:a :href ,url ,@others ,@body)
      `(<:a :href ,url ,@others ,url)))

(deftag-macro <:stylesheet (sheet &allow-other-attributes others)
  `(<:link :rel "stylesheet" :href ,sheet ,@others))

(deftag-macro <:text (&allow-other-attributes others)
  `(<:input :type "text" ,@others))

(deftag-macro <:submit (&allow-other-attributes others)
  `(<:input :type "submit" ,@others))

(deftag-macro <:image (&allow-other-attributes others)
  `(<:input :type "image" ,@others))

(deftag-macro <:checkbox (&allow-other-attributes others)
  `(<:input :type "checkbox" ,@others))

(deftag-macro <:file (&allow-other-attributes others)
  `(<:input :type "file" ,@others))

(deftag <:&nbsp () (emit-princ "&nbsp;"))

(def-html-tag <:marquee
  width
  height
  direction
  behaviour
  scrolldelay
  scrollamount
  loop
  bgcolor
  hspace
  vspace)

(def-html-tag <:applet code archive width height)

(deftag <:param (&attribute name value)
  (emit-empty-tag "param" `("name" ,name
                            "value" ,value)))
