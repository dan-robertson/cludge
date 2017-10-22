(cl:in-package #:cl)

;;; we use this package to name primops
(defpackage #:js-special
  (:use #:common-lisp)
  #|(:use #:cleavir-primop)|#
  (:shadow #:boundp)
  (:export
   #:set-fdefinition
   ;; (set-fdefinition newvalue name)
   #:set-sfdefinition
   ;; (set-sfdefinition newvalue name)
   #:operator
   ;; e.g (operator js:+ a b)
   #:dot
   ;; e.g (dot thing "length") or (dot thing foo)
   #:if-js
   ;; e.g (if-js then else) only evaluates then when in compiled JS. vice-versa for else.
   #:add-global-special
   ;; e.g. (add-global-special 'name)
   #:boundp
   ;; e.g. (boundp 'name)
   #:set-symbol-value
   ;; e.g. (set-symbol-value 'name new-value)
   ))

(defpackage #:js-impl
  (:use :common-lisp))

(defpackage #:js-environment
  (:nicknames #:js-env)
  (:use #:cl)
  (:export #:compilation-environment))

(defpackage #:js-relooper
  (:use #:cl)
  (:shadow #:block #:condition #:type)
  (:export #:add-block #:add-branch #:code #:with-relooper))

(defpackage #:js-cleavir
  (:use #:cl #:js-environment)
  (:shadow #:compile)
  (:export #:js-system #:*js-system* #:*js-interpreter-system*
	   #:compile
	   #:compile-stream))
