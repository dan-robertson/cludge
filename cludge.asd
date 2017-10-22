#|
  This file is a part of the cludge project.
  Copyright (c) 2016 Dan Robertson <dan.robertson14@gmail.com>
|#

#|
  Author: Dan Robertson <dan.robertson14@gmail.com>
|#

(cl:in-package #:cl-user)

(in-package #:asdf-user)

(defsystem #:cludge
  :version "0.1"
  :author "Dan Robertson"
  :license ""
  :depends-on (:sicl-simple-environment
	       :sicl-environment
	       :cleavir-environment
	       :cleavir-ast
	       :cleavir-ast-interpreter
	       :cleavir-generate-ast
	       :cleavir-code-utilities

	       :sicl-reader-simple

	       :sicl-evaluation-and-compilation
	       :sicl-data-and-control-flow-support
	       :sicl-conditionals-support
	       :sicl-arithmetic
	       :sicl-loop-support
	       :sicl-cons-support
	       :sicl-iteration-support
	       :sicl-clos-support
	       :sicl-conditions

	       :cleavir-ast-to-hir
	       :cleavir-ir
	       :cleavir-hir-transformations
	       :cleavir-basic-blocks
	       )
  :components ((:module "src"
                :components
                ((:file "packages")
		 (:file "js-syntax")
		 (:file "js-system" :depends-on ("packages" "js-syntax"))
		 (:file "js-environment" :depends-on ("packages" "js-system"))
		 (:file "relooper-classes" :depends-on ("packages"))
		 (:file "relooper" :depends-on ("packages" "js-syntax" "relooper-classes"))
		 (:file "js-cleavi2" :depends-on ("packages" "js-syntax" "js-system"
							     "js-environment" "relooper"))
		 ;; (:file "js-cleavir" :depends-on ("packages" "js-syntax" "js-system" "js-environment"))
		 )))
  :description "A proof of concept partially-implemented Common Lisp to JS compiler"
  :long-description "")
