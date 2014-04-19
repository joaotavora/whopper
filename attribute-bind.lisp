;; -*- lisp -*-

(in-package :whopper)

;;;; * ATTRIBUTE-BIND

;; This is used to represent variable in (@ foo) forms. We need it
;; for being able to type-check for it.
;; (let ((foo (list :att1 "val1")))
;;   <tag (@ foo)>)
(defstruct (runtime-attribute-list-reference (:conc-name ralr-))
  (form nil))

;;;; This macro serves the same purpose as destructuring-bind but
;;;; allows for a different syntax.

(defmacro attribute-bind (attribute-spec attribute-values &body body)
  "Evaluate BODY with the values in LIST bound according to ATTRIBUTE-SPEC.

ATTRIBUTE-SPEC has the following form:

 ( required-args* [ &attribute attributes* ] 
                  [ &allow-other-attributes others ]
                  [ &allow-custom-attributes customs ]
                  [ &body body ] )

The symbols in REQUIRED-ARGS will be positionaly bound to the
values in LIST. After the required args have been consumed any
keyword value pairs will be consumed and bound to the
corresponding attributes (binding form is just like &key in
regular lambda lists, but only keyword symbols are allowed).

If &allow-other-attributes is present then OTHERS will be bound
to a list containing all the attributes in LIST which don't have
a corresponding &attribute variable. &allow-other-attributes implies
&allow-custom-attributes and OTHERS will contain also the custom
attributes.

If &allow-custom-attributes is present then CUSTOMS will be bound
to a list containing all the custom attributes provided in (@ ...)
sections.

if &body is present then BODY will be bound to anything remaining
in LIST after attribute parsing is complete."
  (destructuring-bind (locals attrs flags other-attributes
                       custom-attributes body-var)
      (parse-attribute-spec attribute-spec)
    (when (and other-attributes
               (not custom-attributes))
      (setf custom-attributes (gensym "CUSTOM-ATTRIBUTES")))
    (alexandria:with-unique-names (element)
      (rebinding (attribute-values)
        `(let ,(remove-if #'null (append locals
                                         attrs
                                         flags
                                         (list other-attributes)
                                         (list custom-attributes)
                                         (list body-var)))
           ,@(when body-var
               `((declare (ignorable ,body-var))))
           ,@(loop
               for local in locals
               collect `(setf ,local (pop ,attribute-values)))
           (setf ,attribute-values
                 (loop for el :in ,attribute-values
                       if (and (listp el)
                               (symbolp (first el))
                               (string= "@" (first el)))
                         do ,(if custom-attributes
                                 `(setf ,custom-attributes
                                        (append ,custom-attributes
                                                (if (cddr el)
                                                    (rest el)
                                                    (list (make-runtime-attribute-list-reference
                                                           :form (second el))))))
                                 `(error 'illegal-attribute-use :attribute-type "custom"))
                       else
                         collect el))
           (loop while (and (consp ,attribute-values)
                            (keywordp (car ,attribute-values)))
                 do (let ((,element (pop ,attribute-values)))
                      (case ,element
                        ,@(loop
                            for attr in attrs
                            ;; NB: ATTR is (symbol-to-bind-to default-value),
                            ;; we want to match against the keyword whose
                            ;; string name is (symbol-name symbol-to-bind-to),
                            ;; hence the intern.
                            collect `(,(intern (string (car attr)) :keyword) (setf ,(car attr) (pop ,attribute-values))))
                        ,@(loop
                            for flag in flags
                            collect `(,(intern (string flag) :keyword) (setf ,flag t)))
                        (t
                         ,(if other-attributes
                              `(progn
                                 (push ,element ,other-attributes)
                                 (push (pop ,attribute-values) ,other-attributes))
                              `(error 'unrecognized-attribute :attribute ,element))))))
           ,(when (null body-var)
              `(when ,attribute-values
                 (warn "Ignoring extra elements in body: ~S" ,attribute-values)))
           ,(when body-var
              `(setf ,body-var ,attribute-values))
           ,(when other-attributes
              `(setf ,other-attributes (append (nreverse ,other-attributes) ,custom-attributes)))
           ,@(if (and (consp body)
                      (consp (car body))
                      (eql 'declare (car (car body))))
                 `((locally ,@body))
                 body))))))

(define-condition tag-related-error (error)
  ((tag :accessor tag :initarg :tag :initform nil)))

(define-condition unrecognized-attribute (tag-related-error)
  ((attribute :accessor attribute :initarg :attribute))
  (:report (lambda (c s)
             (if (tag c)
                 (format s "Unrecognized attribute ~S in ~S." (attribute c) (tag c))
                 (format s "Unrecognized attribute ~S." (attribute c))))))

(define-condition illegal-attribute-use (tag-related-error)
  ((attribute-type :accessor attribute-type :initarg :attribute-type))
  (:report (lambda (c s)
             (format s "Attributes of type '~A' are not allowed ~A (hint: missing &allow-custom-attributes?)."
                     (attribute-type c)
                     (let ((tag (tag c)))
                       (if tag
                           (format nil "for tag ~A" tag)
                           "here"))))))

(defun parse-attribute-spec (attribute-spec)
  "Parse an attribute spec into required args, attribute args,
  other args and the body arg."
  (let* ((required '())
         (attrs '())
         (flags '())
         (body-var nil)
         (other-attributes nil)
         (custom-attributes nil)
         (put (lambda (item)
                (assert (symbolp item) (item) "Invalid attribute specification at mandatory attribute position: ~A" item)
                (push item required))))
    (dolist (attr attribute-spec)
      ;; the #'string= tom-follery (god i love that word) is so that
      ;; the & symbols can be read in from any package. we're kinda
      ;; faking keywords...
      (if (symbolp attr)
	  (cond
	    ((string= attr '&attribute)
	     (setf put (lambda (item)
			 (if (listp item)
			     (case (length item)
			       (1 (push (list (first item) nil) attrs))
			       (2 (push item attrs))
			       (t (error "Bad &attribute spec: ~S" item)))
			     (push (list item nil) attrs)))))
	    ((string= attr '&flag)
	     (setf put (lambda (item)
			 (push item flags))))
	    ((string= attr '&body)
	     (setf put (lambda (item)
			 (setf body-var item))))
	    ((string= attr '&allow-other-attributes)
	     (setf put (lambda (item)
			 (setf other-attributes item))))
            ((string= attr '&allow-custom-attributes)
	     (setf put (lambda (item)
			 (setf custom-attributes item))))
	    (t (funcall put attr)))
	  (funcall put attr)))
    (list (nreverse required) (nreverse attrs) (nreverse flags)
          other-attributes custom-attributes body-var)))
