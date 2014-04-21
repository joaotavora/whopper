;; -*- lisp -*-

(defpackage :whopper-system
  (:use :common-lisp :asdf))

(in-package :whopper-system)

(defsystem :whopper
  :serial t
  :components ((:file "package")
               (:file "utils")
               (:file "attribute-bind")
               (:file "http")
               (:file "whopper")
               (:module :tags
                :serial t
                :components ((:file "html4")
                             (:file "html5")
                             (:file "standard-whopper")
                             (:file "html+"))))
  :depends-on (:alexandria))

(defsystem :whopper-test
  :components ()
  :depends-on (:whopper :stefil))

;; (defmethod perform ((op asdf:test-op) (system (eql (find-system :whopper))))
;;   (asdf:oos 'asdf:load-op :whopper-test)
;;   (funcall (read-from-string "run-package-tests") :whopper-test))


;;;; * Introduction

;;;; WHOPPER is a library for creating HTML in lisp.

;;;;@include "src/packages.lisp"

;;;;@include "src/whopper.lisp"

;;;;@include "src/attribute-bind.lisp"

;;;;@include "src/tags/html4.lisp"

;;;;@include "src/tags/html+.lisp"

;;;;@include "src/tags/standard-whopper.lisp"
