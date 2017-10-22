;;; Don't run this. These are just notes

(eval-when (:compile-toplevel :load-toplevel :execute)
  (error "Don't load this file"))

;;; Dependencies:

(asdf:load-system '#:sicl-simple-environment)

(asdf:load-system '#:cleavir-ast)
(asdf:load-system '#:cleavir-generate-ast)
(asdf:load-system '#:cleavir-ast-to-hir)
(asdf:load-system '#:cleavir-hir)
(asdf:load-system '#:cleavir-hir-to-mir) ;NOTE: hir-to-mir is in the cleavir-ir package
(asdf:load-system '#:cleavir-mir)

;;; special variable, representing the way the form is being compiled. determines properties of e.g. eval-when
(let ((cleavir-generate-ast:*compiler* 'cl:eval))
  ;...
  )
(let ((cleavir-generate-ast:*compiler* 'cl:compile))
  ;...
  )
(let ((cleavir-generate-ast:*compiler* 'cl:compile-file))
  ;...
  )

;;; the system parameter:
;;; We get to pass around a system parameter to various compilation functions
;;; and can use it to implement our own methods on cleavir generic functions.
;;; Plan so far:
;;; a cludge-system which e.g. converts all floats to double-floats
;;; a cludge-compile-time-system (inheriting from cludge-system) which we use to
;;;   compile forms so that the compiler can evaluate them. e.g used to compile things
;;;   in (eval-when (:compile-toplevel) ... )
;;; a cludge-js-system (inheriting from cludge-system) which we use to compile forms
;;;   to JS. (e.g basically every form)
;;; These two systems correspond (AFAICT) to the extrinsic-environment and runtime-environment



;;; main way to start compiling:
;;; environment is a class representing e.g. macro definitions
(cleavir-generate-ast:ast-from-file filename environment system)
(cleavir-generate-ast:ast-from-stream stream environment system)
(cleavir-generate-ast:generate-ast form environment system)

;;; graphviz stuff
;;; PATH-VAR is bound to a pathname to which some graphviz graph may be written.
;;; After BODY is done, graphviz is run and then evince is opened on the file
(defmacro with-dot-then-view (path-var &body body)
	   (let ((x (gensym)))
	   `(uiop:with-temporary-file (:pathname ,x :type "ps")
	      (uiop:with-temporary-file (:pathname ,path-var)
		,@body
		(uiop:run-program (list "dot" "-Tps" (princ-to-string ,path-var)) :output ,x)
		(uiop:run-program (list "evince" (princ-to-string ,x)))))))

;;; Example:
(defparameter *my-env* (make-instance 'sicl-simple-environment:simple-environment))
(cleavir-generate-ast:generate-ast '(+ 1 2) *my-env* nil) ;restart with assume-global
(with-dot-then-view x (cleavir-ast-graphviz:draw-ast * x))
(cleavir-ast-transformations:hoist-load-time-value **)
(cleavir-ast-to-hir:compile-toplevel *)
(with-dot-then-view x (cleavir-ir-graphviz:draw-flowchart * x))

;; with hir=**
(cleavir-hir-transformations:eliminate-typeq hir)
(sicl-extrinsic-environment::translate hir *my-env*)
(eval *)
(apply * (mapcar (lambda (x) (cleavir-env:eval x *my-env* *my-env*)) (cleavir-ir:forms hir)))

;;; We want to be able to use the extrinsice environment to correctly compile code for some other
;;; environment. The following is I think currently a problem:
;;; (defmacro foo (x) (let ((y x)) `(values ,(lambda () (incf y)) ,(lambda () (decf y)))))
;;; The reason this is a problem is that to compile the following form to
;;; native code to run extrinsically, we need to have some AST for the lambdas and capture y:
;;; (foo 3)
;;; macroexpansion => (values #<lambda capturing y> #<lambda capturing y>)	(*)
;;; The macroexpander for foo runs in the extrinsic (compilation environment) inside the host system
;;; but we want to compile the form (*) for some native system so we need to be able to generate some
;;; IR with information about captured variables to go with every lambda created in the evaluation
;;; environment.
