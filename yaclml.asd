;; -*- lisp -*-

(defpackage :it.bese.yaclml.system
  (:use :common-lisp :asdf))

(in-package :it.bese.yaclml.system)

(defsystem :yaclml
  :components ((:static-file "yaclml.asd")
               (:file "packages")
               (:file "yaclml" :depends-on ("packages" "attribute-bind"))
               (:file "attribute-bind" :depends-on ("packages"))
               (:module :tags
                :serial t
                :components ((:file "html4")
                             (:file "standard-yaclml")
                             (:file "svg")
                             (:file "html+"))
                :depends-on ("yaclml")))
  :depends-on ())

(defsystem :yaclml.test
  :components ()
  :depends-on (:yaclml :stefil))

;; (defmethod perform ((op asdf:test-op) (system (eql (find-system :yaclml))))
;;   (asdf:oos 'asdf:load-op :yaclml.test)
;;   (funcall (read-from-string "run-package-tests") :it.bese.yaclml.test))


;;;; * Introduction

;;;; YACLML is a library for creating HTML in lisp.

;;;;@include "src/packages.lisp"

;;;;@include "src/yaclml.lisp"

;;;;@include "src/attribute-bind.lisp"

;;;;@include "src/tags/html4.lisp"

;;;;@include "src/tags/html+.lisp"

;;;;@include "src/tags/standard-yaclml.lisp"
