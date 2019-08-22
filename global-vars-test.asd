;;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-lisp; Package: CL-USER -*-

(asdf:defsystem :global-vars/test
  :description "Test suite for global-vars."
  :license "MIT"
  :author "James M. Lawrence <llmjjmll@gmail.com>"
  :depends-on (:global-vars)
  :serial t
  :components ((:file "global-vars-test")))

(defmethod perform ((o asdf:test-op) (c (eql (asdf:find-system :global-vars/test))))
  (declare (ignore o c))
  (funcall (intern (string '#:run) :global-vars/test)))
