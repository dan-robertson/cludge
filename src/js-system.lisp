(cl:in-package #:js-cleavir)

(defclass js-system () () (:documentation "Representing the javascript system"))

(defparameter *js-system* (make-instance 'js-system))

(defclass js-interpreter-system () ()
  (:documentation "Representing the pseudo-javascript system for compile-time eval"))

(defparameter *js-interpreter-system* (make-instance 'js-interpreter-system))

(deftype js-float-type ()
  (first
   '(#+sbcl double-float
     double-float)))

(deftype js-fixnum-type ()
  '(signed-byte 32))

;;; TODO: maybe similar methods for cleavir-hir-transformations:convert-constant-to-immediate

;;; So that all floats become (immediate) double floats
(defmethod cleavir-generate-ast:convert-constant-to-immediate ((constant float) env (system js-system))
  (declare (ignore env system))
  (coerce constant 'js-float-type))

;;; we have 32-bit fixnums (so bitwise ops work on them)
;;; We convert those to immediates
(defmethod cleavir-generate-ast:convert-constant-to-immediate ((constant integer) env (system js-system))
  (declare (ignore env system))
  (if (typep constant 'js-fixnum-type)
      constant
      nil))


(defmethod cleavir-generate-ast:check-special-form-syntax
    ((head (eql 'js-special:if-js)) form)
  (cleavir-code-utilities:check-form-proper-list form)
  (cleavir-code-utilities:check-argcount form 2 2))

(defmethod cleavir-generate-ast:convert-special
    ((head (eql 'js-special:if-js)) form env (system js-system))
  (cleavir-generate-ast:convert (second form) env system))

(defmethod cleavir-generate-ast:convert-special
    ((head (eql 'js-special:if-js)) form env (system js-interpreter-system))
  (cleavir-generate-ast:convert (third form) env system))


;;; we implement setf functions as symbol.s instead of trying to find fdefinition of (setf name)
(defclass sfdefinition-ast (cleavir-ast:fdefinition-ast) ()
  (:documentation "like fdefinition-ast but for (setf name)"))
(defclass sfdefinition-instruction (cleavir-ir:fdefinition-instruction) ()
  (:documentation "an fdefinition instruction for (fdefinition (setf name))"))

(defmethod cleavir-generate-ast:convert-global-function (info global-env (system js-system))
  (declare (ignore global-env))
  (let* ((name (cleavir-env:name info))
	 (true-name (if (consp name) (second name) name))
	 (ast (cleavir-ast:make-fdefinition-ast
	       (cleavir-ast:make-load-time-value-ast `',true-name t)
	       info)))
    (if (consp name)
	(change-class ast 'sfdefinition-ast)
	ast)))

(defmethod cleavir-ast-interpreter::interpret-ast ((ast sfdefinition-ast) env)
  (let ((name (cleavir-ast-interpreter::interpret-ast (cleavir-ast:name-ast ast) env)))
    (sicl-genv:fdefinition `(setf ,name) js-env::*interpreter-global-environment*)))

(defmethod cleavir-ast-to-hir:compile-ast ((ast sfdefinition-ast) context)
  (cleavir-ast-to-hir:check-context-for-one-value-ast context)
  (let* ((temp (cleavir-ast-to-hir:make-temp)))
    (cleavir-ast-to-hir:compile-ast
     (cleavir-ast:name-ast ast)
     (cleavir-ast-to-hir:context
      (list temp)
      (list
       (change-class
	(cleavir-ir:make-fdefinition-instruction
	 temp
	 (first (cleavir-ast-to-hir::results context))
	 (first (cleavir-ast-to-hir::successors context)))
	'sfdefinition-instruction))
      (cleavir-ast-to-hir::invocation context)))))

(defclass set-fdefinition-ast (cleavir-ast:ast cleavir-ast:no-value-ast-mixin)
  ;; evaluates object-ast first
  ((name-ast :initarg :name-ast :accessor cleavir-ast:name-ast)
   (object-ast :initarg :object-ast :accessor cleavir-ast:object-ast)
   (info :initarg info :accessor cleavir-ast:info)))

(defclass set-sfdefinition-ast (set-fdefinition-ast)
  ())
;;; inputs are 1. name, 2. value
(defclass set-fdefinition-instruction (cleavir-ir:instruction
				       cleavir-ir:one-successor-mixin
				       cleavir-ir:side-effect-mixin)
  ())

(defclass set-sfdefinition-instruction (set-fdefinition-instruction)
  ())

(defmethod cleavir-ast:children ((ast set-fdefinition-ast))
  (list (cleavir-ast:name-ast ast) (cleavir-ast:object-ast ast)))

(defmethod cleavir-generate-ast:check-special-form-syntax
    ((head (eql 'js-special:set-fdefinition)) form)
  (cleavir-code-utilities:check-form-proper-list form)
  (cleavir-code-utilities:check-argcount form 2 2))
(defmethod cleavir-generate-ast:check-special-form-syntax
    ((head (eql 'js-special:set-sfdefinition)) form)
  (cleavir-code-utilities:check-form-proper-list form)
  (cleavir-code-utilities:check-argcount form 2 2))

(defmethod cleavir-generate-ast:convert-special ((head (eql 'js-special:set-fdefinition))
						 form environment system)
  (declare (ignore head))
  (destructuring-bind (newvalue name) (cdr form)
    (make-instance 'set-fdefinition-ast
		   :object-ast (cleavir-generate-ast:convert newvalue environment system)
		   :name-ast (cleavir-generate-ast:convert name environment system))))
(defmethod cleavir-generate-ast:convert-special ((head (eql 'js-special:set-sfdefinition))
						 form environment system)
  (declare (ignore head))
  (destructuring-bind (newvalue name) (cdr form)
    (make-instance 'set-sfdefinition-ast
		   :object-ast (cleavir-generate-ast:convert newvalue environment system)
		   :name-ast (cleavir-generate-ast:convert name environment system))))

;;; inputs are name then new-value
(defmethod cleavir-ast-interpreter::interpret-ast ((ast set-fdefinition-ast) env)
  (declare (special js-env::*interpreter-global-environment*))
  (let ((new (cleavir-ast-interpreter::interpret-ast (cleavir-ast:object-ast ast) env))
	(name (cleavir-ast-interpreter::interpret-ast (cleavir-ast:name-ast ast) env)))
    (setf (sicl-genv:fdefinition name js-env::*interpreter-global-environment*)
	  new)))
(defmethod cleavir-ast-interpreter::interpret-ast ((ast set-sfdefinition-ast) env)
  (declare (special js-env::*interpreter-global-environment*))
  (let ((new (cleavir-ast-interpreter::interpret-ast (cleavir-ast:object-ast ast) env))
	(name (cleavir-ast-interpreter::interpret-ast (cleavir-ast:name-ast ast) env)))
    (setf (sicl-genv:fdefinition `(setf ,name) js-env::*interpreter-global-environment*)
	  new)))

(defmethod cleavir-ast-to-hir:compile-ast ((ast set-fdefinition-ast) context)
  (let ((temp1 (cleavir-ast-to-hir:make-temp))
	(temp2 (cleavir-ast-to-hir:make-temp)))
    (cleavir-ast-to-hir:compile-ast
     (cleavir-ast:object-ast ast)
     (cleavir-ast-to-hir:context
      (list temp1)
      (list (cleavir-ast-to-hir:compile-ast
	     (cleavir-ast:name-ast ast)
	     (cleavir-ast-to-hir:context
	      (list temp2)
	      (list (make-instance 'set-fdefinition-instruction
				   :inputs (list temp2 temp1)
				   :outputs '()
				   :successors (cleavir-ast-to-hir::successors context)))
	      (cleavir-ast-to-hir::invocation context))))
      (cleavir-ast-to-hir::invocation context)))))
(defmethod cleavir-ast-to-hir:compile-ast ((ast set-sfdefinition-ast) context)
  (let ((temp1 (cleavir-ast-to-hir:make-temp))
	(temp2 (cleavir-ast-to-hir:make-temp)))
    (cleavir-ast-to-hir:compile-ast
     (cleavir-ast:object-ast ast)
     (cleavir-ast-to-hir:context
      (list temp1)
      (list (cleavir-ast-to-hir:compile-ast
	     (cleavir-ast:name-ast ast)
	     (cleavir-ast-to-hir:context
	      (list temp2)
	      (list (make-instance 'set-sfdefinition-instruction
				   :inputs (list temp2 temp1)
				   :outputs '()
				   :successors (cleavir-ast-to-hir::successors context)))
	      (cleavir-ast-to-hir::invocation context))))
      (cleavir-ast-to-hir::invocation context)))))


;;; TODO: maybe change to be at least 2 args
(defclass js-operator-ast (cleavir-ast:ast cleavir-ast:one-value-ast-mixin)
  ((operator :initarg :operator :accessor operator)
   (left-ast :initarg :left-ast :accessor left-ast)
   (right-ast :initarg :right-ast :accessor right-ast)))
(defclass js-boolean-operator-ast (js-operator-ast cleavir-ast:boolean-ast-mixin)
  ())

(defmethod cleavir-ast:children ((ast js-operator-ast))
  (list (left-ast ast) (right-ast ast)))
;;; This acts as both a normal operator and a test depending on the number of successors
;;; TODO: maybe make two instructions
(defclass js-operator-instruction (cleavir-ir:instruction)
  ((operator :initarg :operator :accessor operator)))

(defmethod cleavir-ast:children ((ast js-operator-ast))
  (list (left-ast ast) (right-ast ast)))

(defmethod cleavir-generate-ast:check-special-form-syntax
    ((head (eql 'js-special:operator)) form)
  (cleavir-code-utilities:check-form-proper-list form)
  (cleavir-code-utilities:check-argcount form 3 3)
  (assert (and (symbolp (second form))
	       (eq (symbol-package (second form))
		   (find-package '#:js-syntax)))
	  () "js-special:operator needs its operator to be a symbol from JS-SYNTAX"))

(defmethod cleavir-generate-ast:convert-special
    ((head (eql 'js-special:operator)) form env system)
  (destructuring-bind (op left right) (cdr form)
    (ecase op
      ((js:+ js:- js:/ js:* js:\| js:& js:^ js:~
	     js:%)
       (make-instance 'js-operator-ast
		      :operator op
		      :left-ast (cleavir-generate-ast:convert left env system)
		      :right-ast (cleavir-generate-ast:convert right env system)))
      ((js:== js:=== js:< js:<= js:> js:>=
	      js:!= js:!== js:&& js:\|\|
	      js:in js:instanceof)
       (make-instance 'js-boolean-operator-ast
		      :operator op
		      :left-ast (cleavir-generate-ast:convert left env system)
		      :right-ast (cleavir-generate-ast:convert right env system))))))

(defmethod cleavir-ast-to-hir:compile-ast ((ast js-operator-ast) context)
  (let ((temp1 (cleavir-ast-to-hir:make-temp))
	(temp2 (cleavir-ast-to-hir:make-temp)))
    (cleavir-ast-to-hir:compile-ast
     (left-ast ast)
     (cleavir-ast-to-hir:context
      (list temp1)
      (list (cleavir-ast-to-hir:compile-ast
	     (right-ast ast)
	     (cleavir-ast-to-hir:context
	      (list temp2)
	      (list (make-instance 'js-operator-instruction
				   :operator (operator ast)
				   :inputs (list temp1 temp2)
				   :outputs '()
				   :successors (cleavir-ast-to-hir::successors context)))
	      (cleavir-ast-to-hir::invocation context))))
      (cleavir-ast-to-hir::invocation context)))))

(defclass js-dot-ast (cleavir-ast:ast cleavir-ast:one-value-ast-mixin)
  ((left-ast :initarg :left-ast :accessor left-ast)
   (right :initarg :right :accessor right)  ;either an AST, a string or a number
   (constant :initarg :constant :accessor dot-constant)))
(defmethod children ((ast js-dot-ast))
  (cons (left-ast ast)
	(let ((r (right ast)))
	  (typecase r
	    (string nil)
	    (number nil)
	    (cleavir-ast:ast (list r))))))
;;; if this has non-nil property then does (first input).property otherwise (first input).(second input)
(defclass js-dot-instruction (cleavir-ir:instruction cleavir-ir:one-successor-mixin)
  ((property :initarg :property :accessor property)))

(defmethod cleavir-generate-ast:check-special-form-syntax
    ((head (eql 'js-special:dot)) form)
  (cleavir-code-utilities:check-form-proper-list form)
  (cleavir-code-utilities:check-argcount form 2 2))
(defmethod cleavir-generate-ast:convert-special
    ((head (eql 'js-special:dot)) form env system)
  (destructuring-bind (left right) (cdr form)
    (typecase right
      ((or string number)
       (make-instance 'js-dot-ast
		      :left-ast (cleavir-generate-ast:convert left env system)
		      :right right
		      :constant t))
      (t
       (make-instance 'js-dot-ast
		      :left-ast (cleavir-generate-ast:convert left env system)
		      :right (cleavir-generate-ast:convert right env system)
		      :constant nil)))))
(defmethod cleavir-ast-to-hir:compile-ast ((ast js-dot-ast) context)
  (let ((temp1 (cleavir-ast-to-hir:make-temp)))
    (if (dot-constant ast)
	(cleavir-ast-to-hir:compile-ast
	 (left-ast ast)
	 (cleavir-ast-to-hir:context
	  (list temp1)
	  (list (make-instance 'js-dot-instruction
			       :property (right ast)
			       :inputs (list temp1)
			       :outputs (cleavir-ast-to-hir::results context)
			       :successors (cleavir-ast-to-hir::successors context)))
	  (cleavir-ast-to-hir::invocation context)))
	(let ((temp2 (cleavir-ast-to-hir:make-temp)))
	  (cleavir-ast-to-hir:compile-ast
	   (left-ast ast)
	   (cleavir-ast-to-hir:context
	    (list temp1)
	    (list (cleavir-ast-to-hir:compile-ast
		   (right ast)
		   (cleavir-ast-to-hir:context
		    (list temp2)
		    (list (make-instance 'js-dot-instruction
					 :property nil
					 :inputs (list temp1 temp2)
					 :outputs (cleavir-ast-to-hir::results context)
					 :successors (cleavir-ast-to-hir::successors context)))
		    (cleavir-ast-to-hir::invocation context))))
	    (cleavir-ast-to-hir::invocation context)))))))

(defclass js-add-global-special-ast (cleavir-ast:ast cleavir-ast:one-value-ast-mixin)
  ((name-ast :initarg :name-ast :accessor name-ast)))
(defmethod cleavir-ast:children ((ast js-add-global-special-ast))
  (list (name-ast ast)))

(defclass js-add-global-special-instruction (cleavir-ir:instruction
					     cleavir-ir:one-successor-mixin
					     cleavir-ir:side-effect-mixin)
  ())

(defclass js-boundp-ast (cleavir-ast:ast cleavir-ast:boolean-ast-mixin)
  ((name-ast :initarg :name-ast :accessor name-ast)))
(defmethod cleavir-ast:children ((ast js-boundp-ast))
  (list (name-ast ast)))

(defclass js-boundp-instruction (cleavir-ir:instruction cleavir-ir:two-successors-mixin)
  ())

(defmethod cleavir-generate-ast:check-special-form-syntax
    ((head (eql 'js-special:add-global-special)) form)
  (cleavir-code-utilities:check-form-proper-list form)
  (cleavir-code-utilities:check-argcount form 1 1))

(defmethod cleavir-generate-ast:convert-special
    ((head (eql 'js-special:add-global-special)) form env system)
  (let ((name-form (second form)))
    (make-instance 'js-add-global-special-ast
		   :name-ast (cleavir-generate-ast:convert name-form env system))))

(defmethod cleavir-generate-ast:check-special-form-syntax
    ((head (eql 'js-special:boundp)) form)
  (cleavir-code-utilities:check-form-proper-list form)
  (cleavir-code-utilities:check-argcount form 1 1))

(defmethod cleavir-generate-ast:convert-special
    ((head (eql 'js-special:boundp)) form env system)
  (let ((name-form (second form)))
    (make-instance 'js-boundp-ast
		   :name-ast (cleavir-generate-ast:convert name-form env system))))

(defmethod cleavir-generate-ast:check-special-form-syntax
    ((head (eql 'js-special:set-symbol-value)) form)
  (cleavir-code-utilities:check-form-proper-list form)
  (cleavir-code-utilities:check-argcount form 2 2))

(defmethod cleavir-generate-ast:convert-special
    ((head (eql 'js-special:set-symbol-value)) form env system)
  (destructuring-bind (symbol newvalue) (cdr form)
    (let ((name (cleavir-ast:make-lexical-ast (gensym)))
	  (value (cleavir-ast:make-lexical-ast (gensym))))
      (cleavir-generate-ast::process-progn
       (list (cleavir-ast:make-setq-ast name (cleavir-generate-ast:convert symbol env system))
	     (cleavir-ast:make-setq-ast value (cleavir-generate-ast:convert newvalue env system))
	     (cleavir-ast:make-set-symbol-value-ast name value)
	     value)))))

(defmethod cleavir-ast-to-hir:compile-ast ((ast js-add-global-special-ast) context)
  (cleavir-ast-to-hir:check-context-for-one-value-ast context)
  (let* ((temp (cleavir-ast-to-hir:make-temp)))
    (cleavir-ast-to-hir:compile-ast
     (name-ast ast)
     (cleavir-ast-to-hir:context
      (list temp)
      (list
       (make-instance 'js-add-global-special-instruction
		      :inputs (list temp)
		      :outputs (cleavir-ast-to-hir::results context)
		      :successors (cleavir-ast-to-hir::successors context)))
      (cleavir-ast-to-hir::invocation context)))))
;;; TODO this should be a conditional
(defmethod cleavir-ast-to-hir:compile-ast ((ast js-boundp-ast) context)
  (cleavir-ast-to-hir::check-context-for-boolean-ast context)
  (let* ((temp (cleavir-ast-to-hir:make-temp)))
    (cleavir-ast-to-hir:compile-ast
     (name-ast ast)
     (cleavir-ast-to-hir:context
      (list temp)
      (list
       (make-instance 'js-boundp-instruction
		      :inputs (list temp)
		      :outputs (cleavir-ast-to-hir::results context)
		      :successors (cleavir-ast-to-hir::successors context)))
      (cleavir-ast-to-hir::invocation context)))))
