(in-package :whopper)

(defmacro def-handlebars-tag (name &rest attributes)
  (let ((effective-attributes (make-effective-attributes attributes)))
    (alexandria:with-unique-names (custom-attributes)
      `(deftag ,name (&attribute ,@effective-attributes
                                 &allow-custom-attributes ,custom-attributes &body body)
         (let ((*tag-markers* '("{{" "}}" "/")))
           (emit-princ (first *tag-markers*))
           (emit-princ name)
           (mapc #'emit-princ-attributes attributes)
           (emit-princ (second *tag-markers*))
           (emit-open-tag ,(string-downcase (symbol-name name))
                          (list ,@(loop for attr in effective-attributes
                                        collect (string-downcase (symbol-name attr))
                                        collect attr))
                          ,custom-attributes)
           (emit-body body)
           (emit-close-tag ,(string-downcase (symbol-name name))))))))

(defpackage :whopper-handlebars
  (:nicknames #:{)
  (:export #:input))

(def-handlebars-tag {:input type id placeholder value action)

(with-whopper-output-to-string ({:input))
