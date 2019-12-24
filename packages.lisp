;;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-COMMON-LISP; Package: CL-USER -*-

(defpackage #:global-vars
  (:export #:define-global-var
           #:define-global-var*
           #:define-global-parameter
           #:define-global-parameter*)
  (:use :cl))

(defpackage #:global-vars/test
  (:use :cl :global-vars)
  (:export #:run))

