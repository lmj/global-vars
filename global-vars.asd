;;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-lisp; -*-

(defsystem :global-vars
  :version "1.0.0"
  :description "Define efficient global variables."
  :license "MIT"
  :author "James M. Lawrence <llmjjmll@gmail.com>"
  :serial t
  :components ((:file "packages")
	       (:file "global-vars")))

(defmethod perform ((o test-op) (c (eql (find-system :global-vars))))
  (declare (ignore o c))
  (load-system '#:global-vars/test)
  (test-system '#:global-vars/test))

(defmethod perform :after ((o asdf:load-op)
                           (c (eql (find-system :global-vars))))
  (declare (ignore o c))
  (pushnew :global-vars *features*))
