;; -*- Mode: Lisp; indent-tabs-mode:nil -*-

(in-package :common-lisp-user)

(defpackage :whopper
  (:nicknames :whopper)
  (:documentation "Yet Another Common Lisp Markup Language")
  (:use :common-lisp)
  (:export

   ;; code generation
   #:emit-princ
   #:emit-html
   #:emit-code
   #:emit-princ-attributes
   #:emit-open-tag
   #:emit-close-tag
   #:emit-empty-tag
   #:emit-body
   #:emit-xml-tag
   #:wrap-in-tag

   ;; defining tags
   #:deftag
   #:deftag-macro

   ;; using whopper
   #:with-whopper-stream
   #:with-whopper-output-to-string
   #:enable-whopper-syntax
   #:disable-whopper-syntx
   #:enable-xml-syntax
   #:disable-xml-syntax
   #:with-xml-syntax
   #:*whopper-stream*
   #:*whopper-indent*

   #:href

   #:+xhtml-strict-doctype+
   #:+xhtml-transitional-doctype+
   #:+xhtml-frameset-doctype+
   ))

(defpackage :whopper-tags
  (:nicknames :<)
  (:documentation "WHOPPER programmatic HTML generation.")
  (:use)
  (:export
   ;; HTML4
   #:a #:abbr #:acronym #:address #:area #:b #:base #:bdo #:big
   #:blockquote #:body #:br #:button #:caption #:cite #:code #:col
   #:colgroup #:dd #:del #:dfn #:div #:dl #:dt #:em #:fieldset #:form
   #:frame #:frameset #:h1 #:h2 #:h3 #:h4 #:h5 #:h6 #:head #:hr #:html
   #:i #:iframe #:img #:input #:ins #:kbd #:label #:legend #:li #:link
   #:map #:meta #:noframes #:noscript #:object #:ol #:optgroup #:option
   #:p #:param #:pre #:q #:samp #:script #:select #:small #:span
   #:strong #:style #:sub #:sup #:table #:tbody #:td #:textarea #:tfoot
   #:th #:thead #:title #:tr #:tt #:ul #:var
   ;; Not really HTML4, but close enough
   #:applet #:param #:marquee #:embed
   ;; WHOPPER extended HTML
   #:href #:stylesheet #:text #:submit #:image #:checkbox #:file
   #:as-is #:as-html #:call-with-whopper-stream #:comment #:progn
   #:&nbsp #:format
   ;; whopper+ (shortcuts)
   #:ah #:ai))

;; the nicknames for these packages take care the svg tags
;; below don't signal an error.  because of symbol names and xml
;; format incompatibilities.
;; TODO clean up, we have the (@ ...) syntax now. see also svg.lisp.

(defpackage :whopper-xml
  (:use)
  (:nicknames :xml)
  (:export
   #:id
   #:base
   #:lang
   #:space))

(defpackage :whopper-xlink
  (:use)
  (:nicknames :xlink)
  (:export
   #:type
   #:href
   #:role
   #:arcrole
   #:title
   #:show
   #:actuate))

(defpackage :whopper-svg
  (:use :cl)
  (:documentation "SVG library.")
  (:nicknames :svg :<svg))
