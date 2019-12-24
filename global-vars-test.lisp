;;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-lisp; Package: GLOBAL-VARS-TEST -*-

(in-package #:global-vars-test)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *a-call-count* 0)
  (defun a ()
    (incf *a-call-count*)
    :a))

(define-global-var -a- (a) "a")
(define-global-var -a- 99)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *b-call-count* 0)
  (defun b ()
    (incf *b-call-count*)
    :b))

(define-global-parameter -b- (b) "b")
(define-global-parameter -b- 99)

(defparameter *c-call-count* 0)
(defun c ()
  (incf *c-call-count*)
  :c)

(define-global-var* -c- (c) "c")
(define-global-var* -c- 99)

(defparameter *d-call-count* 0)
(defun d ()
  (incf *d-call-count*)
  :d)

(define-global-parameter* -d- (d) "d")
(define-global-parameter* -d- 99)

(define-global-var -e- 99)

(defun run ()
  (assert (eq :a -a-))
  (assert (= 1 *a-call-count*))
  (assert (string= (documentation '-a- 'variable) "a"))
  (assert (eq 99 -b-))
  (assert (= 1 *b-call-count*))
  (assert (string= (documentation '-b- 'variable) "b"))
  (assert (eq :c -c-))
  (assert (= 1 *c-call-count*))
  (assert (string= (documentation '-c- 'variable) "c"))
  (assert (eq 99 -d-))
  (assert (= 1 *d-call-count*))
  (assert (string= (documentation '-d- 'variable) "d"))
  (assert (null (documentation '-e- 'variable)))
  (assert (eq '-w- (define-global-parameter -w- 0)))
  (assert (eq '-x- (define-global-parameter* -x- 0)))
  (assert (eq '-y- (define-global-var -y- 0)))
  (assert (eq '-z- (define-global-var* -z- 0)))
  (format t "~&global-vars tests passed.~%")
  (values))
