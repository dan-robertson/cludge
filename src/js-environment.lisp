(cl:in-package #:js-env)

(defclass compilation-environment
    (sicl-simple-environment:simple-environment)
  ())

;;; Notes for implementation:
;;; We need multiple environments:
;;; - Something to hold definitions of functions which we can evaluate at compile time
;;; - Something to put definitions into at run time
;;;
;;; TODO: SICL refers to a compilation-environment and a linkage-environment
;;; determine what these are.
;;;
;;; Linkage environments are (at least partly) explained in
;;; SICL/Code/Compiler/Extrinsic-environment/load.lisp
;;; Quotation:
;;; This version of the function LOAD takes two environment objects.
;;; See section 3.2.1 in the HyperSpec for a description of the role
;;; of COMPILATION-ENVIRONMENT.  The LINKAGE-ENVIRONMENT is similar to
;;; what is called the RUN-TIME environment in section 3.2.1.  For
;;; SICL, it is more appropriate to call it the LINKAGE-ENVIRONMENT.
;;; It is the environment used to look up runtime definitions of
;;; functions and variables, whereas the code could very well be
;;; executed in a different environment.
;;;
;;; Our environments must support: (looking at sicl-simple-environment:simple-environment)
;;; global function definitions (getting and setting)
;;; global setf function definitions (getting and setting)
;;; global constants/special variables (getting, setting and letting)
;;; symbol property-lists 
;;; class definitions
;;; macro definitions (maybe not in runtime (?))
;;; compiler macro definitions (maybe not in runtime (?))
;;; default setf expander (maybe not in runtime (?))
;;; packages
;;; proclamations (maybe not in runtime)
;;; setf-expanders (maybe not in runtime (?))
;;; standard-method-combination (not sure what this is for yet)
;;; type expanders (maybe not in runtime (?))
;;; unbound (not sure what this is)
;;; 
;;; Our runtime environment (implemented in JS) must also support:
;;; catch tags
;;; unwind-protects
;;; 
;;; plan for JS:
;;; have a dynamic environment object passed to functions
;;; it needs to store special variables. We can do this well with Object.create
;;; it needs to store catch-tags and unwind-protects.
;;;  these can be closures which know their own dynamic environments
;;; it needs to support being able to unwind to a specific points (e.g. nonlocal goto)
;;; perhaps throw/catch should be used for unwind-protect/catch-tags
;;; but this would interfere with being fully CPS (and other goto stuff)


(defun import-special-forms (env)
  "Defines the usual common lisp special operators to be special in env"
  (loop for op in '(block catch eval-when flet function go if labels let let*
		    load-time-value locally macrolet multiple-value-call
		    multiple-value-prog1 progn progv quote
		    return-from setq symbol-macrolet tagbody the throw unwind-protect
		    
		    cleavir-primop:car
		    cleavir-primop:cdr

		    js-special:if-js
		    
		    js-special:set-fdefinition
		    js-special:set-sfdefinition
		    js-special:operator
		    js-special:dot
		    js-special:add-global-special
		    js-special:boundp
		    js-special:set-symbol-value

		    )
       do (setf (sicl-genv:special-operator op env) t)))


;;; to determine if a symbol names a special variable
;;; we check whether its value is dynamic by letting it twice
;;; and having an function in the outer scope called from the inner scope
;;; to see whether the inner (dynamic) or outer (lexical) scope applies in the function.
;;; The problem is that this can conflict with declared types
;;; (e.g. setting *standard-output* to 1) and throw a type error.
;;; We assume that type-checking implies that the variable is special
(defun special-variable-p (name)
  "Determines whether NAME names a special variable"
  (if (constantp name) (return-from special-variable-p nil))
  (let ((f (gensym)) (x (gensym)) (y (gensym)))
    (handler-case
	(handler-bind
	    ((warning #'muffle-warning))
	  (funcall
	   (compile nil
		    `(lambda (,x ,y)
		       (declare (optimize (safety 0)))
		       (let ((,name ,x))
			 (flet ((,f () ,name))
			   (let ((,name ,y))
			     (eql (,f) ,name))))))
	   1 2))
      (type-error (e)
	(declare (ignore e))
	t))))

(defun acceptable-package-p (pack)
  (or (eq pack (find-package '#:common-lisp))
      (let* ((name (package-name pack))
	     (l (length name)))
	(or (string-equal "sicl-" name :end2 (min l 5))
	    (string-equal "cleavir-" name :end2 (min l 8))
	    (string-equal "js-" name :end2 (min l 3))))))

(defun import-lisp (env)
  "Imports lisp functions and special variables from common-lisp, sicl and cleavir"
  ;; import packages
  (setf (sicl-genv:packages env) (remove-if-not #'acceptable-package-p (list-all-packages)))
  ;; import the things defined on symbols
  (do-all-symbols (s)
    (when (acceptable-package-p (symbol-package s))
      (cond
	((constantp s)
	 (unless (sicl-genv:boundp s env)
	   (setf (sicl-genv:constant-variable s env)
		 (symbol-value s))))
	((special-variable-p s)
	 (let ((boundp (boundp s)))
	   (setf (sicl-genv:special-variable s env boundp)
		 (if boundp (symbol-value s))))))
      (cond
	((special-operator-p s))
	((macro-function s))
	((fboundp s)
	 (setf (sicl-genv:fdefinition s env)
	       (fdefinition s))))
      (cond ((fboundp `(setf ,s))
	     (setf (sicl-genv:fdefinition `(setf ,s) env)
		   (fdefinition `(setf ,s))))))))


(defun import-js (env)
  "Imports some JS constants and types"
  ;; TODO: float types -- we only have IEEE doubles CLHS says they should be called single-float
  (setf (sicl-genv:constant-variable 'most-positive-fixnum env) 2147483647)
  (setf (sicl-genv:constant-variable 'most-negative-fixnum env) -2147483648)
  )

;;; we need to define EVAL for compilation-environments. We do this by making an AST
;;; and then interpreting it. See also: cleavir-ast-interpreter

(defvar *interpreter-global-environment*) ;set by the entry point into the interpreter

;;; this is a modification for cleavir-ast-interpreter

;;; TODO: need to check for special definitions
;;; TODO: need to do set-symbol-value
(defmethod cleavir-ast-interpreter::interpret-ast
    ((ast cleavir-ast:symbol-value-ast) env)
  ;; we need to use *interpreter-global-environment*
  (let ((symbol (cleavir-ast-interpreter::interpret-ast (cleavir-ast:symbol-ast ast) env)))
    ;; check for constant
    (multiple-value-bind (val pres)
	(sicl-genv:constant-variable symbol *interpreter-global-environment*)
      ;; otherwise check for special bindings
      (loop for e in env
	 do (multiple-value-bind (val present)
		(gethash symbol e)
	      (when present
		(return-from cleavir-ast-interpreter::interpret-ast val))))
      ;;otherwise we check the global environment
      (if pres val
	  (multiple-value-bind (val pres)
	      (sicl-genv:special-variable symbol *interpreter-global-environment*)
	    (if pres val
		(error 'unbound-variable :name symbol)))))))
(defmethod cleavir-ast-interpreter::interpret-ast
    ((ast cleavir-ast:set-symbol-value-ast) env)
  ;; we need to use *interpreter-global-environment*
  (let ((symbol (cleavir-ast-interpreter::interpret-ast (cleavir-ast:symbol-ast ast) env))
	(new-value (cleavir-ast-interpreter::interpret-ast (cleavir-ast:value-ast ast) env)))
    ;; check for constant
    (multiple-value-bind (val pres)
	(sicl-genv:constant-variable symbol *interpreter-global-environment*)
      (declare (ignore val))
      ;; now check for special bindings
      (loop for e in env
	 do (multiple-value-bind (val present)
		(gethash symbol e)
	      (declare (ignore val))
	      (when present
		(return-from cleavir-ast-interpreter::interpret-ast
		  (setf (gethash symbol e) new-value)))))
      (if pres (error "Cannot set constant ~s" symbol)
	  (setf (sicl-genv:special-variable symbol *interpreter-global-environment* t)
		new-value)))))
(defmethod cleavir-ast-interpreter::interpret-ast ((ast cleavir-ast:fdefinition-ast) env)
  (let ((name (cleavir-ast-interpreter::interpret-ast (cleavir-ast:name-ast ast) env)))
    (sicl-genv:fdefinition name *interpreter-global-environment*)))
;;; we add this so that the global-environment for the interpreter is preserved when the
;;; function leaves the dynamic extent of INTERPRET
(defmethod cleavir-ast-interpreter::interpret-ast :around ((ast cleavir-ast:function-ast) env)
  (let ((fun (call-next-method))
	(genv *interpreter-global-environment*))
    (lambda (&rest arguments)
      (let ((*interpreter-global-environment* genv))
	(apply fun arguments)))))
;; we don't store machine integers in immediate ASTs -- we just store the value they become
(defmethod cleavir-ast-interpreter::interpret-ast ((ast cleavir-ast:immediate-ast) env)
  (cleavir-ast:value ast))

(defun interpret (ast evaluation-environment eval)
  (let ((*interpreter-global-environment* evaluation-environment))
    ;we can't use cleavir-ast-interpreter:interpret as we need our own eval
    (check-type ast cleavir-ast:top-level-function-ast "a top-level function AST")
    (let ((f (cleavir-ast-interpreter::interpret-ast ast '())))
      (apply f
	     (mapcar eval (cleavir-ast:forms ast))))))

(defmethod cleavir-env:eval (form environment1 (environment2 compilation-environment))
  (cond
    ((atom form) form)
    ((and (consp form)
	  (consp (cdr form))
	  (null (cddr form))
	  (eq (car form) 'quote))
     (cadr form))
    ;; TODO: maybe non-nil system (e.g. to convert all floats to double-float)
    (t
     (let ((cleavir-generate-ast:*compiler* 'cl:eval))
       (let* ((ast (cleavir-generate-ast:generate-ast form environment1
						      js-cleavir:*js-interpreter-system*))
       					;environment1 contains lexical macros
	      (hoisted (cleavir-ast-transformations:hoist-load-time-value ast)))
	 (interpret hoisted
		    environment2 ;environment2 doesn't contain lexical variables
		    (lambda (form) (cleavir-env:eval form environment1 environment2))))))))

(defun import-file (path env)
  (with-open-file (s path)
    (let ((*package* (sicl-genv:special-variable '*package* env))) ;TODO: other reader variables?
      (loop with eof = (list 'eof)
	 for form = (sicl-reader:read s nil eof) ;we use this reader as we know what we get for `, ,
	 until (eq form eof)
	 do (cleavir-env:eval
	     form env env		;i.e. in null lexical scope
	     )
	   (setf *package* (sicl-genv:special-variable '*package* env))))))

(defun import-sicl-file (name env)
  (let ((path (asdf:system-relative-pathname :sicl-extrinsic-environment name)))
    (import-file path env)))

(defun import-js-file (name env)
  (let ((path (asdf:system-relative-pathname :cludge name)))
    (import-file path env)))

(defmethod initialize-instance :after ((x compilation-environment) &rest args)
  (declare (ignore args))
  (import-js x)
  (import-special-forms x) ;primops too?
  (import-lisp x) ;some SICL stuff?

  (setf (sicl-genv:special-variable 'sicl-genv:*global-environment* x t) x)
  (setf (sicl-genv:fdefinition 'sicl-genv:global-environment x) (lambda () x))

  ;; We need to define defmacro and stuff now.
  ;; we use the sicl reader so we need to define the backquote macros now
  (setf (sicl-genv:fdefinition 'sicl-reader::expand x)
	(fdefinition 'sicl-reader::transform)) ;this line comes from extrinsic-env/define-backquote
  (setf (sicl-genv:macro-function 'sicl-reader::quasiquote x)
	(macro-function 'sicl-reader::quasiquote))
  (setf (sicl-genv:macro-function 'defmacro x)
	(compile nil
		 (cleavir-code-utilities:parse-macro
		  'defmacro
		  '(name lambda-list &body body)
		  `((eval-when (:compile-toplevel :load-toplevel :execute)
		      (setf (sicl-genv:macro-function name ,x)
			    (compile nil
				     (cleavir-code-utilities:parse-macro
				      name lambda-list body))))))))
  (setf (sicl-genv:macro-function 'in-package x)
	(lambda (form env)
	  (declare (ignore env))
	  (setq *package* (find-package (cadr form)));not really sure we need this line
	  `(setq *package* (find-package ',(cadr form)))))
  (setf (sicl-genv:default-setf-expander x)
	(lambda (form)
	  (if (symbolp form)
	      (let ((n (gensym)))
		(values '() '() `(,n) `(setq ,form ,n) form))
	      (let ((temps (loop for a in form collect (gensym))))
		(values (rest temps)
			(rest form)
			(list (first temps))
			`(funcall #'(setf ,(first form)) ,@temps)
			`(,(first form) ,@(cdr temps)))))))
  (import-sicl-file "../../Evaluation-and-compilation/lambda.lisp" x)
  (import-sicl-file "../../Environment/multiple-value-bind.lisp" x) ;TODO: maybe our own mvbind special
  (import-sicl-file "../../Data-and-control-flow/setf.lisp" x)
  (import-sicl-file "../../Environment/defmacro-defmacro.lisp" x)
  (import-sicl-file "../../Environment/defmacro-defmacro.lisp" x)
  (import-sicl-file "../../Environment/in-package.lisp" x)
  (import-sicl-file "../../Evaluation-and-compilation/lambda.lisp" x)
  (import-sicl-file "../../Data-and-control-flow/multiple-value-list.lisp" x)
  (import-sicl-file "../../Data-and-control-flow/nth-value.lisp" x)
  (import-sicl-file "../../Environment/defun-defmacro.lisp" x)
  (import-sicl-file "../../Data-and-control-flow/get-setf-expansion.lisp" x)
  (import-sicl-file "../../Conditionals/macros.lisp" x)
  (import-sicl-file "../../Environment/standard-environment-macros.lisp" x)
  (import-sicl-file "../../Evaluation-and-compilation/macroexpand.lisp" x)
  (import-sicl-file "../../Evaluation-and-compilation/macro-function.lisp" x)
  (import-sicl-file "../../Evaluation-and-compilation/declaim-defmacro.lisp" x)
  (import-sicl-file "../../Environment/standard-environment-functions.lisp" x)
  (import-sicl-file "../../Arithmetic/incf-decf-defmacro.lisp" x)
  (import-sicl-file "../../Loop/loop-defmacro.lisp" x)
  (import-sicl-file "../../Cons/push-pop-defmacro.lisp" x)
  (import-sicl-file "../../Data-and-control-flow/return-defmacro.lisp" x)
  (import-sicl-file "../../Data-and-control-flow/prog1-prog2-defmacro.lisp" x)
  (import-sicl-file "../../Data-and-control-flow/prog-progstar-defmacro.lisp" x)
  (import-sicl-file "../../Data-and-control-flow/psetq-defmacro.lisp" x)
  (import-sicl-file "../../Data-and-control-flow/psetf-defmacro.lisp" x)
  (import-sicl-file "../../Data-and-control-flow/rotatef-defmacro.lisp" x)
  (import-sicl-file "../../Data-and-control-flow/destructuring-bind-defmacro.lisp" x)
  (import-sicl-file "../../Data-and-control-flow/shiftf-defmacro.lisp" x)
  (import-sicl-file "../../Cons/pushnew-defmacro.lisp" x)
  (import-sicl-file "../../Iteration/dotimes-defmacro.lisp" x)
  (import-sicl-file "../../Iteration/dolist-defmacro.lisp" x)
  (import-sicl-file "../../Iteration/do-dostar-defmacro.lisp" x)
  ;;(import-sicl-file "../../Stream/with-open-stream-defmacro.lisp" x)
  (import-sicl-file "../../CLOS/with-slots-defmacro.lisp" x)
  (import-sicl-file "../../CLOS/with-accessors-defmacro.lisp" x)
  (import-sicl-file "../../Cons/remf-defmacro.lisp" x)
  (import-sicl-file "../../CLOS/defgeneric-defmacro.lisp" x)
  (import-sicl-file "../../Conditions/assert-defmacro.lisp" x)
;;  (import-sicl-file "../../CLOS/defmethod-defmacro.lisp" x) ;this doesn't work right now
  (import-sicl-file "../../CLOS/defclass-support.lisp" x)
  (import-sicl-file "../../CLOS/defclass-defmacro.lisp" x)

  (import-js-file "src/js-impl.lisp" x)
  (import-js-file "src/js-impl-final.lisp" x)

  (setf (sicl-genv:special-variable '*package* x t) (find-package '#:cl-user))
  )
;;; 
