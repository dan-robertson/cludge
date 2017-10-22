(cl:in-package #:js-cleavir)


;;; some constants relating to how we represent some CL objects in JS
(defconstant +symbol-function-tag+ "f" "symbol.f == (fdefinition symbol)")
(defconstant +symbol-setf-function-tag+ "s" "symbol.s == (fdefinition `(setf ,symbol))")

(defconstant +cons-car-tag+ "a" "cons.a == (car cons)")
(defconstant +cons-cdr-tag+ "d" "cons.d == (cdr cons)")

(defconstant +cell-contents-tag+ "x" "cell.x == (read-cell cell)")

(defconstant +symbol-maker-function-name+ '(js:global "symbol")
  "Called with args (name, package-name, external=false) to get a symbol interned in that package")

(defconstant +most-positive-32bit-integer+ 2147483647)
(defconstant +most-negative-32bit-integer+ -2147483648)

(deftype js-32bit-integer ()
  `(integer ,+most-negative-32bit-integer+ ,+most-positive-32bit-integer+))
(deftype js-53bit-integer ()
  `(integer ,(- 1 (expt 2 53)) ,(- (expt 2 53) 1)))


;;; some variables to hold symbols referring to special variables for our runtime
(defvar *value-passing-var* '(js:global "extra_values")
  "variable which is used to pass extra values back to caller")
(defvar *dynamic-scope-var* nil
  "variable which holds the current dynamic scope")
(defvar *nil-var* '(js:global "nil")
  "variable holds a reference to the constant NIL")
(defvar *t-var* '(js:global "T"))


(defvar *compile-function-function*)
(setf (documentation '*compile-function-function* 'variable)
      "A function which given some enter instruction returns js-syntax representing
that function.")

(defvar *datum-name-map*) ; a hash-table taking datums to their JS representations


;;; for debugging
(defmacro with-dot-then-view (path-var &body body)
  (let ((x (gensym)))
    `(uiop:with-temporary-file (:pathname ,x :type "ps")
       (uiop:with-temporary-file (:pathname ,path-var)
	 ,@body
	 (uiop:run-program (list "dot" "-Tps" (princ-to-string ,path-var)) :output ,x)
	 (uiop:run-program (list "evince" (princ-to-string ,x)))))))
(defun graphviz (hir)
  (with-dot-then-view x (cleavir-ir-graphviz:draw-flowchart hir x)))


(defun compile (form compilation-environment)
  (compile-ast (cleavir-generate-ast:generate-ast form compilation-environment *js-system*)
	       compilation-environment))

(defun compile-stream (stream compilation-environment)
  (let ((cleavir-generate-ast:*compiler* 'cl:compile-file))
    (let ((ast
	   (cleavir-generate-ast::process-progn
	    (let ((*package* (sicl-genv:special-variable '*package* compilation-environment)))
	      ;TODO: other reader variables?
	      (loop with eof = (list 'eof)
		 for form = (sicl-reader:read stream nil eof) ;has control of `, ,
		 until (eq form eof)
		 collect (cleavir-generate-ast:generate-ast form compilation-environment *js-system*)
		 do (setf *package* (sicl-genv:special-variable '*package* compilation-environment)))))))
      (compile-ast ast compilation-environment))))

(declaim (inline egethash))
(defun egethash (key ht datum &rest args)
  "like GETHASH but instead of returning multiple values, signals an error if the key is not present"
  (multiple-value-bind (val pres) (gethash key ht)
    (if pres val
	(apply #'error datum args))))

(defun compile-ast (ast compilation-environment)
  (declare (ignore compilation-environment))
  (declare (optimize (debug 3)))
  (let* (#|(hoisted (cleavir-ast-transformations:hoist-load-time-value ast))|#
	 ;; We don't want to manually close over load-time-values so we just make
	 ;; a toplevel form and hoist load-time-values later
	 (toplevel (cleavir-ast:make-top-level-function-ast ast '() '()))
	 (hir (cleavir-ast-to-hir:compile-toplevel toplevel))
	 ;; cleavir stuff goes here:
	 ;; js specialisations go here:
	 (ignorej1 (hoist-unwinds hir))
	 (ignorej2 (convert-unwinds hir))
	 ;; as we implement closures ourselves using .bind(), we need to capture the unwind tags
	 (ignorec1 (cleavir-hir-transformations:process-captured-variables hir))
	 (ignorej3 (convert-enter-instructions hir))
	 ;; The above makes constant inputs so we don't need to close over them
	 ;; Now we clean up and hoist
	 (ignorep1 (cleavir-hir-transformations:eliminate-load-time-value-inputs hir *js-system*))
	 (ignorep2 (cleavir-hir-transformations:eliminate-superfluous-temporaries hir))
	 (owners (cleavir-hir-transformations:compute-location-owners hir))
	 )
    (declare (ignore ignorec1
		     ignorej1 ignorej2 ignorej3
		     ignorep1 ignorep2))
    (let ((basic-blocks (make-hash-table)) ;maps owner-> list of basic blocks (main last owner)
	  (*datum-name-map* (make-hash-table))
	  (compiled-functions-js) ;list of js:named-function's
	  (compiled-functions (make-hash-table))) ;maps enter->symbol
      (mapc (lambda (x) ;x = (first last owner)
	      (multiple-value-bind (main last owner)
		  (decode-basic-block x)
		(push (list main last owner) (gethash owner basic-blocks))))
	    (cleavir-basic-blocks:basic-blocks hir))
      (labels ((compile-function (enter-instruction)
		 (multiple-value-bind (val pres) (gethash enter-instruction compiled-functions)
		   (if pres
		       (if val val
			   (error "Function trying to enclose its encloser"))
		       (let ((s (gensym)))
			 (setf (gethash enter-instruction compiled-functions) s)
			 (push 
			  (let* ((*dynamic-scope-var* (gensym "DYN-SCOPE"))
				 (body (compile-function-body-and-vars enter-instruction)))
			    `(js:named-function ,s
						,(cons
						  *dynamic-scope-var*
						  (loop for x in (cleavir-ir:lambda-list enter-instruction)
						     ;; we skip the first output as we treat the
						     ;; static environment specially
						     append (cond
							      ;;these are handled in the body
							      ((member x '(&rest &key))
							       (loop-finish))
							      ((member x '(&optional))
							       nil)
							      ((atom x) (list (translate-datum x)))
							      ((= (length x) 1)
							       (list (translate-datum (first x))))
							      ((= (length x) 2)
							       (list (translate-datum (first x)))))))
						,@body))
			  compiled-functions-js)
			 s))))
	       (compile-function-body-and-vars (enter-instruction)
		 (cons (compile-function-vars enter-instruction)
		       (js-unblock (compile-function-body enter-instruction))))
	       (compile-function-vars (enter-instruction)
		 (list*
		  'js:var
		  (loop with ignore = (loop for x in (cleavir-ir:outputs enter-instruction)
					 if (atom x) collect x
					 else append x)
		     for location being each hash-key of owners
		     using (hash-value owner)
		     when (and (eq owner enter-instruction) (not (member location ignore)))
		       if (typep location 'cleavir-ir:values-location)
 		         append (if (need-both-values location)
				    (translate-datum location)
				    (list (first (translate-datum location))))
		       else
		         append (let ((x (translate-datum location))) (if (symbolp x) (list x)))
		       end
		     end)))
	       (compile-function-body (enter-instruction)
		 ;;todo: iterate owners and make variable definitions
		 (let* ((switch-block (loop for basic-block in
					   (gethash enter-instruction basic-blocks)
					 for (nil last nil) = basic-block
					 when (typep last 'unwind-switch-instruction)
					 return basic-block))
			(switch-inst (and switch-block (second switch-block))))
		   (when switch-block
		     ;; we need to calculate js so that the switch block acts like the main
		     ;; block
		     ;; we replace with our own new basic block
		     (setf (gethash enter-instruction basic-blocks)
			   (nsubstitute
			    (list nil switch-inst enter-instruction)
			    switch-block
			    (gethash enter-instruction basic-blocks))))
		   (multiple-value-bind (code branch-var)
		       (relooper-function-blocks enter-instruction
						 (or switch-inst enter-instruction))
		     (if switch-inst
			 (let ((prelude (loop for inst in (first switch-block)
					   collect (do-translate-instruction inst)))
			       (exception (gensym))
			       (loop-name (gensym)))
			   (js-sequence
			    (apply #'js-sequence prelude)
			    `(js:while (js:number 1)
			       (js:try
				,(js-block code)
				(js:catch ,exception
				  (js:= ,branch-var ,exception)
				  (js:switch
				   (,exception)
				   (js:default) (js:throw ,exception)
				   ,@(loop for in in (cleavir-ir:inputs switch-inst)
					collect `(js:case ,(translate-datum in)))
				   (js:continue ,loop-name))))
			       ,loop-name)))
			 (js-block code)))))
	       (relooper-function-blocks (enter-instruction first-instruction)
		 (let (branch-var
		       (blocks (make-hash-table))) ;maps first-instruction -> block
		   (values
		    (js-relooper:with-relooper (rl)
		      (loop for (main last nil) in (gethash enter-instruction basic-blocks)
			 for first = (if main (car main) last)
			 do (setf (gethash first blocks)
				  (js-relooper:add-block
				   rl
				   (apply #'js-sequence
				    (loop for m in main collect (do-translate-instruction m))))))
		      (loop for (main last nil) in (gethash enter-instruction basic-blocks)
			 for first = (if main (car main) last)
			 do (translate-branch last (egethash first blocks
							     "No block for instruction ~a"
							     first)
					      (mapcar (lambda (i) (egethash i blocks
								      "No block for instruction ~a"
								      i))
						      (cleavir-ir:successors last))
					      (mapcar #'translate-datum (cleavir-ir:inputs last))
					      (mapcar #'translate-datum (cleavir-ir:outputs last))))
		      (setf branch-var
			    (js-relooper::branch-var (gethash first-instruction blocks)))
		      (gethash first-instruction
			       blocks))
		    branch-var))))
	(let ((*dynamic-scope-var* (gensym "DYN-SCOPE")) ;TODO: get from compilation unit
	      (*compile-function-function* #'compile-function))
	  ;; we eval in this twisted order as compiling the body populates compiled-functions-js
	  (let ((main (compile-function-body-and-vars hir)))
	    (append compiled-functions-js
		    main)))))))

;;; our compilation process involves compiling from basic-blocks to JS.
;;; We need to rearrange parts of the ast to allow for UNWIND-INSTRUCTIONS
;;; which are implemented in JS using throw/catch for the unwinding part.
;;; Therefore we need to rewrite the ir to represent this so that this is
;;; represented in the ir.

;;; We then convert these into instructions to create/throw tags and then an
;;; instruction to take all these tags and then switch on them

;;; The first successor is for the non-catching part
;;; The second successor is taken after the throw
(defclass unwind-catch-instruction (cleavir-ir:instruction
				    cleavir-ir:two-successors-mixin)
  ())

;;; takes control to the second successor of the corresponding catcher
(defclass unwind-throw-instruction (cleavir-ir:instruction
				    cleavir-ir:no-successors-mixin
				    cleavir-ir:side-effect-mixin)
  ((catcher :initarg :catcher :reader catcher)))

;;; makes a new tag; zero inputs; one output -- the new tag.
(defclass unwind-make-tag-instruction (cleavir-ir:instruction
				       cleavir-ir:one-successor-mixin)
  ())

;;; throws to the catcher for this tag; one input -- the tag; zero outputs.
(defclass unwind-throw-tag-instruction (cleavir-ir:instruction
					cleavir-ir:no-successors-mixin
					cleavir-ir:side-effect-mixin)
  ())

;;; has n inputs and n-1 successors
;;; when control flows normally to this instruction it goes to the first successor
;;; when a tag is thrown and that tag is the nth input we flow to the (n+1)th successor
;;; when a tag is thrown and that tag is not an input we rethrow it.
(defclass unwind-switch-instruction (cleavir-ir:instruction
					;many successors??
				     )
  ())

;;; Note on implementation of stack unwinding:
;;; We need to be able to unwind the stack to some higher function and then
;;; jump into some part of the body of the higher function. We implement it roughly
;;; like this:
;;; function outer(parameterFunction){
;;;   var x = {}, dynamicExtent = true; label=0;
;;;   L: while(1){
;;;     try{
;;;       dynamicExtent = true;
;;;       switch(label){
;;;       case 0:
;;;         var inner = function(){if(dynamicExtent)throw x;else signal(...) /* this is our unwind */ };
;;;         parameterFunction(inner);
;;;         break L;
;;;       case 1:
;;;         alert("called!");
;;;     } catch(y) {
;;;       switch(y){ case x: label=1; continue L; default: throw y; }
;;;     } finally { dynamcExtent = false; }
;;;   }
;;; We need to create objects to throw and switch on them instead of using e.g. integers
;;; because we can't interfere with either other functions doing a nonlocal transfer
;;; of control or with the function being called recursively (e.g. passing this function
;;; to itself). This stops us from just assigning unique integers catches in different functions

(defmethod cleavir-ir-graphviz:draw-instruction
    ((instruction unwind-throw-instruction) stream)
  (format stream "   ~a [label = \"unwind-throw\"];~%"
	  (cleavir-ir-graphviz::instruction-id instruction))
  (format stream "  ~a -> ~a [color = pink, style = dashed];~%"
	  (cleavir-ir-graphviz::instruction-id instruction)
	  (gethash (catcher instruction)
		   cleavir-ir-graphviz::*instruction-table*)))

(defun hoist-unwinds (initial-instruction)
  (declare (optimize (debug 3)))
  (let ((collections (make-hash-table)))
    (cleavir-ir:map-instructions
     (lambda (instruction)
       (when (typep instruction 'cleavir-ir:unwind-instruction)
	 (push instruction
	       (gethash (cleavir-ir:invocation instruction) collections nil))))
     initial-instruction)
    (loop for enter-instruction being the hash-keys of collections
       using (hash-value list)
       do (loop for unwind in list
	     for s = (first (cleavir-ir:successors enter-instruction))
	     for s2 = (first (cleavir-ir:successors unwind))
	     do (let ((maybe-already-catcher
		       (loop for s* = s then (first (cleavir-ir:successors s*))
			  while (typep s* 'unwind-catch-instruction)
			  when (eq (second (cleavir-ir:successors s*))
				   s2)
			  return s*)))
		  (if maybe-already-catcher
		      (let ((thrower (make-instance 'unwind-throw-instruction
						    :outputs '()
						    :inputs '()
						    :successors '()
						    :predecessors '()
						    :catcher maybe-already-catcher)))
			(print maybe-already-catcher)
			(cleavir-ir:insert-instruction-before thrower unwind)
			(setf (cleavir-ir:successors thrower) ())
			(setf (cleavir-ir:predecessors s2)
			      (remove unwind (cleavir-ir:predecessors s2))))
		      (let* ((catcher (make-instance 'unwind-catch-instruction
						     :outputs '()
						     :inputs '()
						     :successors '()
						     :predecessors '()))
			     (thrower (make-instance 'unwind-throw-instruction
						     :outputs '()
						     :inputs '()
						     :successors '()
						     :predecessors '()
						     :catcher catcher)))
			(cleavir-ir:insert-instruction-after catcher unwind)
			(cleavir-ir:insert-instruction-between catcher
							       enter-instruction s)
			(setf (cleavir-ir:successors catcher) (list s s2))
			(cleavir-ir:insert-instruction-before thrower unwind)
			(setf (cleavir-ir:successors thrower) ())
			(setf (cleavir-ir:predecessors s2)
			      (substitute catcher unwind (cleavir-ir:predecessors s2))))))))
    (labels ((process (instruction)
		(typecase instruction
		  (unwind-catch-instruction
		   (let ((caught-case (second (cleavir-ir:successors instruction))))
		     (if (typep caught-case 'unwind-catch-instruction)
			 (let ((entry (process (first (cleavir-ir:successors instruction))))
			       (old-suc (second (cleavir-ir:successors instruction))))
			   ;; we replace this with the true entry point as that is the branch
			   ;; we must take after catching
			   (setf (cleavir-ir:successors instruction)
				 (list (first (cleavir-ir:successors instruction))
				       entry)
				 (cleavir-ir:predecessors old-suc)
				 (if (eq old-suc (first (cleavir-ir:successors instruction)))
				     (cleavir-ir:predecessors old-suc)
				     (remove instruction
					     (cleavir-ir:predecessors old-suc))))
			   (pushnew instruction (cleavir-ir:predecessors entry)))
			 (process (first (cleavir-ir:successors instruction))))))
		  (t ;some other instruction (we recurse to find the first non-catching instruction
		   instruction))))
      (cleavir-ir:map-instructions
       (lambda (instruction)
	 (when (typep instruction 'cleavir-ir:enter-instruction)
	   (process (first (cleavir-ir:successors instruction)))))
       initial-instruction))
    nil))

(defun convert-unwinds (initial-instruction)
  (declare (optimize (debug 3)))
  (flet ((unwind-switch (unwind-catch-instruction)
	   (multiple-value-bind (entry last-catch)
	       (loop for next = (first (cleavir-ir:successors unwind-catch-instruction))
		         then (first (cleavir-ir:successors next))
		    and prev = unwind-catch-instruction then next
		  unless (typep next 'unwind-catch-instruction)
		  return (values next prev))
	     (if (typep entry 'unwind-switch-instruction)
		 entry
		 (let ((new (make-instance 'unwind-switch-instruction)))
		   (cleavir-ir:insert-instruction-between
		    new
		    last-catch
		    entry)
		   new)))))
    (cleavir-ir:map-instructions
     (lambda (instruction)
       (typecase instruction
	 (unwind-catch-instruction
	  (let ((us (unwind-switch instruction))
		(tag (cleavir-ir:make-shared-location (gensym)))
		(s (cleavir-ir:successors instruction)))
	    (change-class instruction 'unwind-make-tag-instruction
			  :successors (list (first s)))
	    (setf (cleavir-ir:outputs instruction) (list tag))
	    (setf (cleavir-ir:defining-instructions tag) instruction)
	    (setf (cleavir-ir:inputs us)
		  (append (cleavir-ir:inputs us) (list tag));add tag as input to switch
		  (rest (last (cleavir-ir:successors us)))
		  (list (second s)) ;add catching form as successor to switch
		  (cleavir-ir:predecessors (second s))
		  (loop with to-remove = (second s)
		     with to-add = us
		     with found = nil
		     for (pre . rest) on (cleavir-ir:predecessors to-remove)
		     unless (eq pre to-remove)
		     collect pre
		     if (eq pre to-add) do (setf found t)
		     if (and (not rest) (not found)) collect to-add) ;add as predecessor
		  
		  )))
	 (unwind-throw-instruction
	  (assert (typep (catcher instruction) 'unwind-make-tag-instruction))
	  ;; should be guaranteed by traversal order
	  (let ((tag (first (cleavir-ir:outputs (catcher instruction)))))
	    (change-class instruction 'unwind-throw-tag-instruction
			  :inputs (list tag))
	    (pushnew instruction (cleavir-ir:using-instructions tag))))))
     initial-instruction)))

;;; we modify our enter instructions so that they have inputs containing references
;;; to symbols used for keyword parameters
(defclass enter-instruction-with-keys (cleavir-ir:enter-instruction)
  ())
(defun convert-enter-instructions (initial-instruction)
  (cleavir-ir:map-instructions
   (lambda (instruction)
     (when (eq (class-of instruction) 	;i.e. not a subclass
	       (find-class 'cleavir-ir:enter-instruction))
       (change-class instruction 'enter-instruction-with-keys)
       (setf (cleavir-ir:inputs instruction)
	     (loop for arg in (cleavir-ir:lambda-list instruction)
		when (and (consp arg) (= 3 (length arg)))
		collect (let ((key (first arg)))
			  (cleavir-ir:make-load-time-value-input
			   `',key t))))))
   initial-instruction))


;;; Now we need to do stuff with basic blocks
;;; basic blocks from cleavir come in the form
;;; (first last owner)
(defun decode-basic-block (bb)
  "given a basic block bb, returns (values main last owner).
we have bb = (first last owner) and
main is everything from first to last, excluding last"
  (destructuring-bind (first last owner) bb
    (values
     (loop for block = first
	then (car (cleavir-ir:successors block))
	if (eq block last) return main
	else collect block into main)
     last owner)))

(defun js-unblock (thing)
  (cond
    ((null thing) nil)
    ((atom thing) (list thing))
    ((eq 'js:empty (car thing)) nil)
    ((eq 'js:block (car thing)) (cdr thing))
    (t (list thing))))

(defun js-block (parts)
  (cond
    ((null parts) '(js:empty))
    ((null (cdr parts)) (first parts))
    (t (cons 'js:block parts))))

(defun js-sequence (&rest parts)
  (let ((bits (loop for part in parts append (js-unblock part))))
    (js-block bits)))

(defgeneric translate-instruction (instruction input-vars output-vars)
  (:documentation "Translates a given instruction into a valid js-syntax statement.
`instruction' is the instruction to translate.
`input-vars' is a list of the input locations translated to names of js-variables.
If an input location is a values-location then it is represented as a list of two js-variables.
`output-vars' is to `cleavir-ir:outputs' as `input-vars' is to `cleavir-ir:inputs'."))

(defgeneric translate-datum (datum)
  (:documentation "Given a datum translates it into the equivalent JS (to go into input-vars et al)")
  (:method ((datum cleavir-ir:lexical-location))
    (multiple-value-bind (val pres) (gethash datum *datum-name-map*)
      (if pres val
	  (setf (gethash datum *datum-name-map*)
		(gensym)))))
  (:method ((datum cleavir-ir:values-location))
    (multiple-value-bind (val pres) (gethash datum *datum-name-map*)
      (if pres val
	  (setf (gethash datum *datum-name-map*)
		(list (gensym) (gensym))))))
  (:method ((datum cleavir-ir:constant-input))
    (cleavir-ir:value datum))
  (:method ((datum cleavir-ir:immediate-input))
    (cleavir-ir:value datum)))

(defun do-translate-instruction (instruction)
  (let ((inputs (cleavir-ir:inputs instruction))
	(outputs (cleavir-ir:outputs instruction)))
    (translate-instruction instruction
			   (mapcar #'translate-datum inputs)
			   (mapcar #'translate-datum outputs))))

(defgeneric translate-branch (instruction block successor-blocks input-vars output-vars)
  (:documentation "Given an instruction which is the final instruction in a basic block,
as well as the relooper-block for the basic block and the relooper-block for each of the
successors of instruction, this adds branches to block in accordance with instruction")
  (:method ((instruction cleavir-ir:no-successors-mixin) block successor-blocks in out)
    (assert (null successor-blocks))
    (setf (js-relooper:code block)
	  (js-sequence (js-relooper:code block)
		       (do-translate-instruction instruction))))
  (:method ((instruction cleavir-ir:one-successor-mixin) block successor-blocks in out)
    (assert (= 1 (length successor-blocks)))
    (setf (js-relooper:code block)
	  (js-sequence (js-relooper:code block)
		       (do-translate-instruction instruction)))
    (js-relooper:add-branch block (first successor-blocks))))


;;; TRANSLATE-INSTRUCTION methods:

(defmethod translate-instruction ((inst cleavir-ir:nop-instruction) in out)
  '(js:empty))

(defun make-cons (car cdr)
  "Gives the js representation of the cons with given car and cdr"
  `(js:object (,+cons-car-tag+ ,car) (,+cons-cdr-tag+ ,cdr) #|("c" js:true)|#))

(defmethod translate-instruction ((inst cleavir-ir:top-level-enter-instruction) in out)
  ;; (assert (equal (cdr (cleavir-ir:outputs inst)) (cleavir-ir:lambda-list inst)))
  ;; The above assertion seems to fail. Perhaps cleavir-hir-transformation:eliminate-load-time-values
  ;; works differently from cleavir-ast-transformations:hoist-load-time-value
  (assert (null (cleavir-ir:using-instructions (first (cleavir-ir:outputs inst))))
	  ()
	  "Toplevel functions should have no static environment")
  (let ((seen-cells (make-hash-table))) ;we want this so we can avoid problems with circular objects
    (labels
	((translate-form (form &optional top-level-var)
	   (if (atom form)
	       (etypecase form
		 ((not symbol) (translate-object form top-level-var))
		 ((or null keyword) (translate-object form top-level-var)))
	       (ecase (car form)
		 (quote (translate-object (second form) top-level-var)))))
	 (translate-object (object &optional top-level-var)
	   (etypecase object
	     (js-32bit-integer
	      `(js:\| (js:number ,object) (js:number 0)))
	     (js-53bit-integer
	      `(js:number ,object))
	     (real
	      `(js:number ,object))
	     (number (error "Can't make number ~a in JS" object))
	     (null *nil-var*)
	     (symbol `(js:call ,+symbol-maker-function-name+
			       (js:string ,(symbol-name object))
			       (js:string ,(package-name (symbol-package object)))
			       ,(if (eq (nth-value 1 (find-symbol (symbol-name object)
								  (symbol-package object)))
					:external)
				    'js:true
				    'js:false)))
	     (cons
	      (multiple-value-bind (val pres)
		  (gethash object seen-cells)
		(if pres (if val val
			     (error "Can't make circular opject ~a in JS" object))
		    (prog2
			(setf (gethash object seen-cells) nil)
			(make-cons (car object) (cdr object))
		      (setf (gethash object seen-cells) top-level-var)))))
	     (simple-string
	      (multiple-value-bind (val pres)
		  (gethash object seen-cells)
		(if pres
		    (if val val (error "Can't have repeated reference to ~a in JS" object))
		    (progn
		      (setf (gethash object seen-cells) top-level-var)
		      `(js:string ,object))))))
	   ))
      (list*
       'js:var 				;TODO js:const?
       `(,*dynamic-scope-var* (js:call (js:\. (js:global "Object") "create") js:null))
       (loop for var in (cdr out)
	  for form in (cleavir-ir:forms inst)
	  collect `(,var ,(translate-form form var)))))))

(defmethod translate-instruction ((inst enter-instruction-with-keys) in out)
  (let ((lambda-list (cleavir-ir:lambda-list inst))
	(n 1)
	(min-args 0))
    ;; lambda list is of the form:
    ;; locations* [&optional (location exists-location)*] [&rest location]
    ;; [&key (symbol location exists-location)* [&allow-other-keys]]
    ;; We have the default locations and the optional locations populated by the definition
    ;; so we need to set the exists-locations and then handle rest/key/allow-other-keys
    (let* ((result ()) ;we build up a switch statement
	   (vars ())
	   (post ())
	   it
	   (rest-handler (lambda (index-var) (declare (ignore index-var)) nil))
	   (key-handler (lambda (index-var) (declare (ignore index-var)) nil)))
      (flet
	  ((allow-other-keys-p (lambda-list)
	     (member '&allow-other-keys lambda-list)))
	(tagbody
	   ;; NB. building up result backwards so this is in reverse
	   (push '(js:throw (js:string "Not enough arguments"))  ;FIXME: should be CL signal
		 result)
	   (push '(js:case (js:number 0)) result)
	   (push '(js:break) result)
	   (push '(js:case (js:number 1)) result)
	   ;; the above corresponds to switch(arguments.length) {
	   ;; ... TO BE FILLED IN ...
	   ;; case 1: break;
	   ;; case 0: throw "not enough arguments";
	   ;; }
	 normal
	   (case (pop lambda-list)
	     (&optional (go optional))
	     (&rest (go rest))
	     (&key (go key))
	     ((nil) (go end))
	     (t
	      (let ((case (pop result))	    ; ... case x:
		    (breaker (pop result))) ;     break foo; ... throw "not enough arguments"
		(incf min-args)             ; becomes
		(incf n)
		(push case result)          ; case n: break foo; case x: .. throw "not enough arguments"
		(push breaker result)
		(push `(js:case (js:number ,n)) result)
		(go normal))))
	 optional
	   (case (setf it (pop lambda-list))
	     (&rest (go rest))
	     (&key (go key))
	     ((nil) (go end))
	     (t (incf n)
		;; this is our nth lisp arg
		;; but we have a JS arg before them for dynamic environment
		(push `(js:= ,(translate-datum (second it)) ,*t-var*) result)
		(push `(js:case ,n) result)
		(push (list (translate-datum (second it)) *nil-var*) vars)
		(push `(js:if (js:=== ,(translate-datum (second it)) ,*nil-var*)
			      (js:= ,(translate-datum (first it)) ,*nil-var*))
		      post)
		(go optional)))
	 rest
	   (let ((rest-var (translate-datum (pop lambda-list))))
	     (push (list rest-var *nil-var*) vars)
	     (setf rest-handler
		   (lambda (index-var)
		     `((js:= ,rest-var
			     ,(make-cons `(js:\. (js:global "arguments")
						 ,index-var)
					 rest-var)))))
	     (case (pop lambda-list)
	       (&key (go key))
	       ((nil) (go do-rest))))
	 key
	   (let* ((allow-other-keys (allow-other-keys-p lambda-list))
		  (inputs (cleavir-ir:inputs inst))
		  (key-vars
		   (loop for x in lambda-list
		      when (consp x)
		      collect (list (translate-datum (pop inputs))
				    (translate-datum (second x))
				    (translate-datum (third x))))))
	     (loop for (nil a b) in key-vars do (push a vars) (push b vars))
	     ;; as we iterate backwards we check that we are on an even-numbered rest var
	     ;; (i.e. a key) and if it is a key we know 
	     (setf key-handler
		   (lambda (index-var)
		     `((js:if (js:% (js:- ,index-var (js:number ,(- n 1))) (js:number 2))
			      (js:if (js:>= (js:+ ,index-var (js:number 1))
					    (js:\. (js:global "arguments") "length"))
				     ;; FIXME: we need to change this to a CL signal at some point
				     (js:throw (js:string "Odd number of keyword arguments"))
				     (js:switch ((js:\. (js:global "arguments") ,index-var))
					,@(loop for (k var varp) in key-vars
					     collect `(js:case ,k)
					     collect `(js:= ,varp ,*t-var*)
					     collect `(js:= ,var (js:\. (js:global "arguments")
									(js:+ ,index-var
									      (js:number 1))))
					     collect `(js:break))
					,@(if (not allow-other-keys)
					      `((js:default)
						;; FIXME: should be a CL signal
						(js:throw (js:string "Invalid key")))))))))))
	 do-rest
	   (let ((index-var (gensym "I")))
	     (push `(js:for (js:var (,index-var
				     (js:- (js:\. (js:global "arguments") "length") 1)))
			    (js:>= ,index-var (js:number ,n))
			    (js:pre-- ,index-var)
			    ,(js-block
			      (append
			       (funcall rest-handler index-var)
			       (funcall key-handler index-var))))
		   result)
	     (push '(js:default) result)
	     (go real-end))
	 end
	   (push '(js:throw "Too many arguments") result)  ;FIXME: should be a CL signal
	   (push '(js:default) result)
	 real-end))
      `(js:block
	 (js:var (,(first out) js:this) ,@vars)
	 (js:switch ((js:\. (js:global "arguments") "length"))
		    ,@result)
	 ,@post))))

(defun static-environment-element-name (index)
  (assert (integerp index))
  (format nil "c~a" index))

(defun make-static-environment (inputs)
  `(js:object ,@(loop for in in inputs for i from 0
		   collect (list (static-environment-element-name i)
				 in))))

(defmethod translate-instruction ((inst cleavir-ir:enclose-instruction) in out)
  `(js:= ,(first out)
	 ,(if in
	      `(js:call
		(js:\.
		 ,(funcall *compile-function-function* (cleavir-ir:code inst))
		 "bind")
		,(make-static-environment in))
	      (funcall *compile-function-function* (cleavir-ir:code inst)))))

(defmethod translate-instruction ((inst cleavir-ir:fetch-instruction) in out)
  (let* ((place-input (second (cleavir-ir:inputs inst)))
	 (place (etypecase place-input
		  (cleavir-ir:immediate-input
		   (cleavir-ir:value place-input)))))
    `(js:= ,(first out)
	   (js:\. ,(first in) ,(static-environment-element-name place)))))

(defmethod translate-instruction ((inst cleavir-ir:assignment-instruction) in out)
  `(js:= ,(first out) ,(first in)))

(defmethod translate-instruction ((inst cleavir-ir:fdefinition-instruction) in out)
  `(js:= ,(first out) (js:\. ,(first in) ,+symbol-function-tag+)))
(defmethod translate-instruction ((inst sfdefinition-instruction) in out)
  `(js:= ,(first out) (js:\. ,(first in) ,+symbol-setf-function-tag+)))
(defmethod translate-instruction ((inst set-fdefinition-instruction) in out)
  `(js:= (js:\. ,(first in) ,+symbol-function-tag+) ,(second in)))
(defmethod translate-instruction ((inst set-sfdefinition-instruction) in out)
  `(js:= (js:\. ,(first in) ,+symbol-setf-function-tag+) ,(second in)))

(defun need-both-values (values-location)
  (let ((using (cleavir-ir:using-instructions values-location))
	(defining (cleavir-ir:defining-instructions values-location)))
    ;; we don't need multiple values if:
    ;; - we return immediately (then the same variable already holds the extra values)
    ;; - we only call m->f and want one argument
    (not
     (or (and (= 1 (length using))
	      (typep (first using) 'cleavir-ir:return-instruction)
	      (every (lambda (def)
		       (and (typep def 'cleavir-ir:instruction)
			    (= 1 (length (cleavir-ir:successors def)))
			    (eq (first (cleavir-ir:successors def)) (first using))))
		     defining)
	      (notany (lambda (def)
			(typep def 'cleavir-ir:fixed-to-multiple-instruction))
		      defining))
	 (and (every (lambda (u)
		       (and (typep u 'cleavir-ir:multiple-to-fixed-instruction)
			    (= 1 (length (cleavir-ir:outputs u)))))
		     using))))))

(defmethod translate-instruction ((inst cleavir-ir:funcall-instruction) in out)
  (if (need-both-values (first (cleavir-ir:outputs inst)))
      `(js:\, (js:= ,(caar out) (js:call ,(first in) ,*dynamic-scope-var* ,@(rest in)))
	      (js:= ,(cadar out) ,*value-passing-var*))
      `(js:= ,(caar out) (js:call ,(first in) ,*dynamic-scope-var* ,@(rest in)))))

(defmethod translate-instruction ((inst cleavir-ir:return-instruction) in out)
  (if (need-both-values (first (cleavir-ir:inputs inst)))
      `(js:block
	   (js:= ,*value-passing-var* ,(cadar in))
	 (js:return ,(caar in)))
      `(js:return ,(caar in))))

(defmethod translate-instruction ((inst cleavir-ir:multiple-to-fixed-instruction) in out)
  (case (length out)
    (0 '(js:empty))
    (1 `(js:= ,(car out) ,(caar in)))
    ;; for the many args case we get something like this:
    ;; out1=out2=...=outn=NIL
    ;; switch(othervals.length){
    ;; default:
    ;; case n: outn=othervals[n-1];
    ;; case 2: out2=othervals[1];
    ;; case 1: out1=othervals[0];
    ;; }
    ;; out0 = mainval;
    ;; maybe TODO:
    ;; out0=mainval,[out1=nil,out2=nil,...,outn=nil]=othervals
    (t `(js:block
	    `(js:\, (js:= (car out) ,(caar in))
		    (js:= ,@(cdr out) ,*nil-var*))
	  (js:switch
	   ((js:. ,(cadar in) "length"))
	   (js:default)
	   ,@(loop for output in (reverse (cdr out))
	       for n downfrom (length (cdr out))
	       collect `(js:case (js:number ,(1+ n)))
	       collect `(js:= ,output (js:\. ,(cadar in) (js:number ,n)))))))))

(defmethod translate-instruction ((inst cleavir-ir:fixed-to-multiple-instruction) in out)
  (if (need-both-values (first (cleavir-ir:outputs inst)))
      `(js:\, (js:= ,(caar out) ,(if in (car in) *nil-var*))
	      (js:= ,(cadar out) (js:vector ,@(cdr in))))
      `(js:= ,(caar out) ,(if in (car in) *nil-var*))))

(defmethod translate-instruction ((inst unwind-throw-tag-instruction) in out)
  `(js:throw ,(first in)))

(defmethod translate-instruction ((inst unwind-make-tag-instruction) in out)
  `(js:= ,(first out) (js:object)))


(defmethod translate-instruction ((inst cleavir-ir:create-cell-instruction) in out)
  `(js:= ,(first out) (js:object (,+cell-contents-tag+ js:undefined))))

(defmethod translate-instruction ((inst cleavir-ir:write-cell-instruction) in out)
  `(js:= (js:\. ,(first in) ,+cell-contents-tag+)
	 ,(second in)))

(defmethod translate-instruction ((inst cleavir-ir:read-cell-instruction) in out)
  `(js:= ,(first out)
	 (js:\. ,(first in) ,+cell-contents-tag+)))




;;; TRANSLATE-BRANCH methods:

(defmethod translate-branch ((inst unwind-switch-instruction) block successors in out)
  (js-relooper:add-branch block (first successors))
  (loop for suc in (rest successors)
     for tag in in
     do (js-relooper:add-branch block suc `(js:case ,tag)))
  (setf (js-relooper::branch-var block) (gensym)))

(defmethod translate-branch ((inst cleavir-ir:eq-instruction) block successors in out)
  (destructuring-bind (true false) successors
    (js-relooper:add-branch block true `(js:=== ,(first in) ,(second in)))
    (js-relooper:add-branch block false)))
