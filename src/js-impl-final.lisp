;;; Currently when we compile a macro definition (i.e. once we finish
;;; building the environment by eval'ing this file), we only save the
;;; macro definition to the compilation environment and we do not generate
;;; code to save it to the runtime environment.

(cl:in-package #:js-impl)

(defmacro define-compiler-macro (name lambda-list &body body)
  `(eval-when (:compile-toplevel)
     (setf (sicl-genv:compiler-macro-function
	    ',name
	    (load-time-value (sicl-genv:global-environment)))
	   (function ,(cleavir-code-utilities:parse-macro
		       name
		       lambda-list
		       body)))))

(defmacro defmacro (name lambda-list &body body)
  `(eval-when (:compile-toplevel)
     (setf (sicl-env:macro-function ',name sicl-env:*global-environment*)
	   ,(cleavir-code-utilities:parse-macro name lambda-list body))))
