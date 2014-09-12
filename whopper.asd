;; -*- lisp -*-

(defpackage :whopper-system
  (:use :common-lisp :asdf))

(in-package :whopper-system)

(defsystem :whopper
  :components
  ((:module :lib
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
                               (:file "html+"))))))
  :depends-on (:alexandria :babel))

(defsystem :whopper-test
  :components ((:module :test
                :serial t
                :component ((:file "whopper-tests"))))
  :depends-on (:whopper :fiasco))
