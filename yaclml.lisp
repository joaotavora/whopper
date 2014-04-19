;; -*- lisp -*-

(in-package :yaclml)

;;;; * YACLML - Programmatic HTML Generation

;;;; The programmatic interface is a collection of Common Lisp macros
;;;; designed to make embedding HTML in lisp code easy. It was created
;;;; with the following goals in mind:

;;;; - The code for creating HTML should look and act like regular lisp
;;;;   code.

;;;; - Given what we know about HTML and the ratio of static to dynamic
;;;;   text in a typical web page it's important to constant fold as much
;;;;   as possible.

;;;; - Tags should be easily definable and should be able to perform
;;;;   arbitrary computations at both run time and compile time.

;;;; ** Using YACLML Tag Macros

;;;; You use YACLML tags just like regular macros, any attributes are
;;;; passed in like keyword arguments. YACLML examines its args (at
;;;; compile time) and distinguishes between the keyword arguments
;;;; which become attributes and everything else, which becomes the
;;;; tag's body. Tags all have the following syntax:

;;;;  ( tag-name [ :keyword value ] * . body )

;;;; ** Tag Attributes

;;;; The name of the attribute will be the result of string-downcase'ing
;;;; the symbol-name specified in the macro. Depending on the runtime value
;;;; returned executing the value specified in the macro call three things
;;;; can happen:

;;;; NIL - The attribute will be ignored

;;;; T   - The attribute will be printed with (string-downcase name) as
;;;;       the value.

;;;; anything else - The result of evaluating the value will be
;;;;                 printed (via PRINC) as the value of the
;;;;                 attribute.

;;;; If the need ever arises to have an HTML attribute whose value is
;;;; T or NIL it is necessary to return the string \"T\" or \"NIL\"
;;;; and not the symbol T or NIL.

;;;; ** The Tag Body

;;;; Every element of the tag body is processed in order: if it is a
;;;; form it is executed at runtime and must explicitly print to the
;;;; stream *yaclml-stream* if it needs to generate output, if it is a
;;;; string its value will be printed to *yaclml-stream* at run time.

;;;; ** Examples

;;;;   ;; Assuming *yaclml-stream* is bound to *standard-output*

;;;;   (<:a :href \"http://foo.com\" \"foo.com\")
;;;;   => 
;;;;   <a href=\"http://foo.com\">foo.com</a>

;;;;   (<:br)
;;;;   =>
;;;;   <br/>

;;;;   (<:td \"whatever\")
;;;;   =>
;;;;   <td>whatever</td>

(defvar *yaclml-stream* t
  "The stream to which tags are printed.")

(defvar *yaclml-indent* t
  "When T (must be set while compiling yaclml code) the generated
  HTML is indented.")

(defvar %yaclml-indentation-depth% 0)

(defmacro with-yaclml-stream (stream &body body)
  "Evaluate BODY with *yaclml-stream* bound to STREAM."
  `(let ((*yaclml-stream* ,stream))
     (declare (special *yaclml-stream*))
     ,@body))

(defmacro with-yaclml-output-to-string (&body body)
  "Evaluate BODY with *yaclml-stream* bound to a string stream, return the string."
  (alexandria:with-unique-names (output)
    `(with-output-to-string (,output)
       (with-yaclml-stream ,output
         ,@body))))

(defvar %yaclml-code% nil
  "The list of currently collected code this yaclml macro should
   expand into.")

(defvar *expanders* (make-hash-table :test 'eql)
  "Hash table mapping expanders to the expander function.")

(defvar *expander-macros* (make-hash-table :test 'eql)
  "Hash table mapping expander macros to theri macre functions.")

(defun yaclml-constant-p (thing)
  "Returns T if THING is, as far as yaclml is concerned, a run time
  constant."
  (or (stringp thing)
      (characterp thing)
      (numberp thing)
      (keywordp thing)))

(defun emit-princ (&rest items)
  "Emit to the current yaclml-code a form which will, at runtime,
   princ ITEM. If (yaclml-constant-p ITEM) is true the princ will
   be done at compile time."
  (dolist (item items %yaclml-code%)
    (push (cond
            ((stringp item)
             item)
            ((keywordp item)
             (string-downcase (princ-to-string item)))
            ((yaclml-constant-p item)
             (princ-to-string item))
            (t `(princ ,item *yaclml-stream*)))
          %yaclml-code%)))

(defun emit-html (&rest items)
  "Like EMIT-PRINC but escapes html chars in item."
  (dolist (item items %yaclml-code%)
    (if (yaclml-constant-p item)
        (push (escape-as-html (princ-to-string item)) %yaclml-code%)
        (push `(emit-attribute-value ,item) %yaclml-code%))))
  
(defun emit-code (&rest forms)
  "Emit to the current yaclml-code CODE. This means that whatever
   CODE is it will be run, and it's result will be ignored, at
   runtime."
  (setf %yaclml-code% (nconc forms %yaclml-code%)))

(defmacro emit-attribute (name value)
  (rebinding (value)
    `(case ,value
      ((t)
       (princ #\Space *yaclml-stream*)
       (princ ,name *yaclml-stream*)
       (princ "=\"" *yaclml-stream*)
       (princ ,name *yaclml-stream*)
       (princ #\" *yaclml-stream*))
      ((nil) nil)
      (t
       (princ #\Space *yaclml-stream*)
       (princ ,name *yaclml-stream*)
       (princ "=\"" *yaclml-stream*)
       (emit-attribute-value ,value)
       (princ #\" *yaclml-stream*)))))

(defun emit-princ-attribute (name value)
  (unless (stringp name)
    (setf name (string-downcase (princ-to-string name))))
  (emit-code
   (rebinding (value)
     `(case ,value
       ((t)
        (princ ,(concatenate 'string " " name "=\"" name "\"")
         *yaclml-stream*))
       ((nil) nil)
       (t
        (princ ,(concatenate 'string " " name "=\"") *yaclml-stream*)
        (emit-attribute-value ,value)
        (princ "\"" *yaclml-stream*))))))

(defun emit-attribute-value (value)
  (if (listp value)
      (loop for el in value
            for i from 0
            unless (zerop i)
              do (princ #\Space *yaclml-stream*)
            do (write-as-html (princ-to-string el) :stream *yaclml-stream*))
      (write-as-html (princ-to-string value) :stream *yaclml-stream*)))

(defun emit-princ-attributes (attributes)
  "Assuming attributes is a list of (name1 value1 name2 value2 ...), emit
the code necessary to print them at runtime. If VALUE is a
list every element will be concatenated separated by a space to
form the final string value of the attribute.

If the value of any of the attributes is NIL it will be ignored.

If a value is the symbol T the name of the attribute will be used
as the value."
  (loop while attributes
        for key = (pop attributes)
        if (runtime-attribute-list-reference-p key)
          do (emit-code `(loop for (name value) on ,(ralr-form key) by #'cddr
                               unless (stringp name)
                                 do (setf name (string-downcase (string name)))
                               do (emit-attribute name value)))
        else
          do (let ((value (pop attributes)))
               (cond
                 ((eql t value)
                  ;; according to xhtml thoses attributes which in html are
                  ;; specified without a value should just use the attribute
                  ;; name as the xhtml value
                  (emit-princ " " key "=\"" key "\""))
                 ((eql nil value) nil)
                 ((yaclml-constant-p value)
                  (progn
                    (emit-princ " " key "=\"")
                    (emit-html value)
                    (emit-princ "\"")))
                 (t
                  (if (and (consp value)
                           (eql 'cl:concatenate (first value))
                           (consp (cdr value))
                           (eql 'cl:string (second value)))
                      ;; a call to concatenate can be dealt with specially
                      (progn
                        (emit-princ " " key "=\"")
                        (dolist (val (cddr value))
                          (emit-princ val)))
                      (emit-princ-attribute key value))))))
  %yaclml-code%)

(defun emit-indentation ()
  (when *yaclml-indent*
    (emit-princ #\Newline)
    (emit-princ (make-string %yaclml-indentation-depth% :initial-element #\Space))))

(defun emit-open-tag (name &rest attributes)
  "Emit the code required to print an open tag whose name is NAME and
with the attributes ATTRIBUTES. ATTRIBUTES is expected to be an even
long, setf-like list of name-value pairs defining the attributes."
  (emit-princ "<")
  (emit-princ name)
  (mapc #'emit-princ-attributes attributes)
  (emit-princ ">"))

(defun emit-close-tag (name)
  "Emit the code required to print a close tag whose name is NAME."
  (emit-princ "</" name ">"))

(defun emit-empty-tag (name &rest attributes)
  "Emit the code required to print an empty tag with name NAME and a
attributes ATTRIBUTES. See EMIT-OPEN-TAG for more details."
  (emit-princ "<" name)
  (mapc #'emit-princ-attributes attributes)
  (emit-indentation)
  (emit-princ "/>"))

(defun emit-body (body)
  "Traverse body and emit the corresponding code. Every form in body
is analyzed according to the following rules:

cons whose car is not a known expander - code which should be included
with no further analysis.

cons whose car is a known expander - simply call the expander function
with the cdr of the cons as the arg.

yaclml-constant-p - print the constant (after escape-as-html) to
*yaclml-stream*.

cons whose car is YACLML-QUOTE - emit-body on every element of the
cdr.
"
  (cond ((and (not (second body))
              (stringp (first body)))
         (emit-form (first body)))
        (t
         (loop for (form . rest) on body
               do (incf %yaclml-indentation-depth% 2)
                  (emit-indentation)
                  (emit-form form)
                  (decf %yaclml-indentation-depth% 2)
               unless rest
                 do (emit-indentation)))))

(defun emit-form (form)
  "Emits the code to print FORM."
  (if (consp form)
      (let ((op (car form)))
        (cond
          ((gethash op *expander-macros*)
           (emit-form (funcall (gethash op *expander-macros*) (cdr form))))
          ((gethash op *expanders*)
           (funcall (gethash op *expanders*) (cdr form)))
          ((eql 'yaclml-quote op)
           (dolist (b (cdr form))
             (emit-form b)))
          ((eql 'cl:progn op)
           (dolist (b (cdr form))
             (emit-form b)))
          (t (emit-code form))))
      (if (yaclml-constant-p form)
          (emit-princ (escape-as-html (princ-to-string form)))
          (emit-code form))))

(defmacro deftag (name attributes &body body)
  "Define a new tag.

ATTRIBUTES should be an attribute-spec (see parse-attributes and
attribute-bind).

BODY is simply the body of the expander lambda.

Within the BODY the functions EMIT-CODE, EMIT-PRINC and EMIT-HTML can
be used to generate code. EMIT-CODE should be passed lisp code which
will be executed at runtime."
  (alexandria:with-unique-names (contents)
    (when (find-package :swank)
      (destructuring-bind (req keyword-args &rest more)
          (parse-attribute-spec attributes)
        (declare (ignore req more))
        (setf (get name (intern "SWANK-EXTRA-KEYWORDS" :swank))
              keyword-args)))
    `(progn
       (setf (gethash ',name *expanders*)
             (lambda (,contents)
               (handler-bind ((tag-related-error (lambda (c)
                                                   (setf (tag c) ,contents))))
                 (attribute-bind ,attributes ,contents
                   ,@body))))
       (defmacro ,name (&body contents)
         (let ((%yaclml-code% nil)
               (%yaclml-indentation-depth% 0))
           ;; build tag's body
           (funcall (gethash ',name *expanders*) contents)
           (setf %yaclml-code% (nreverse %yaclml-code%))
           ;; now that we've generated the code we can fold the
           ;; strings in yaclml-code and princ them, leaving any other
           ;; forms as they are.
           `(progn ,@(mapcar (lambda (form)
                               (if (stringp form)
                                   `(write-string ,form *yaclml-stream*)
                                   form))
                             (fold-strings %yaclml-code%))
                   (values)))))))

(defmacro deftag-macro (name attributes &body body)
  "Define a new YACLML tag macro.

Tag macros, like regular macros, expand into other YACLML tag
forms which are recursivly processed."
  (let ((contents (gensym))
        (doc-string (if (stringp (first body))
                        (pop body)
                        nil)))
    `(progn
       (setf (gethash ',name *expander-macros*)
             (lambda (,contents)
               (handler-bind ((unrecognized-attribute (lambda (c)
                                                        (setf (tag c) ,contents))))
                 (attribute-bind ,attributes ,contents
                   ,@body))))
       (defmacro ,name (&rest ,contents) ,doc-string
         (funcall (gethash ',name *expander-macros*) ,contents))
       ',name)))

(defmacro def-simple-xtag (name)
  "Convience macro for defing tags which accept any kind of attribute
and just wrap the body in an xml tag."
  `(deftag ,name (&allow-other-attributes other-attributes &body body)
     (if body
         (progn
           (emit-open-tag ,(string-downcase (string name)) other-attributes)
           (emit-body body)
           (emit-close-tag ,(string-downcase (string name))))
         (emit-empty-tag ,(string-downcase (string name)) other-attributes))))

(defmacro wrap-in-tag ((tag-name &rest tag-attributes) &body body)
  (alexandria:with-unique-names (tname)
    `(let ((,tname ,(string-downcase (string tag-name))))
       (emit-open-tag ,tname ,tag-attributes)
       (prog1
           (progn ,@body)
         (emit-close-tag ,tname)))))

;; Copyright (c) 2002-2005, Edward Marco Baringer
;; All rights reserved. 
;; 
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions are
;; met:
;; 
;;  - Redistributions of source code must retain the above copyright
;;    notice, this list of conditions and the following disclaimer.
;; 
;;  - Redistributions in binary form must reproduce the above copyright
;;    notice, this list of conditions and the following disclaimer in the
;;    documentation and/or other materials provided with the distribution.
;;
;;  - Neither the name of Edward Marco Baringer, nor BESE, nor the names
;;    of its contributors may be used to endorse or promote products
;;    derived from this software without specific prior written permission.
;; 
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;; A PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT
;; OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;; SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
;; LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
;; DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
;; THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
;; OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
