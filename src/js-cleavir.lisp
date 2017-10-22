(cl:in-package #:js-cleavir)

(defclass compilation-unit ()
  ((strings :initform nil :accessor strings))
  (:documentation "A class representing the JS source to go into a JS file when output"))

(defvar *compilation-unit*)

;;; lots of constants
;;; many accessors are of the form obj.L where L is one of these
(defconstant +symbol-function-tag+ "f" "symbol.f == (fdefinition symbol)")

;;; main entry point
(defun compile (form compilation-environment
		&optional (compilation-unit (make-instance 'compilation-unit)))
  (let* ((ast (cleavir-generate-ast:generate-ast form compilation-environment *js-system*))
	 (hoisted (cleavir-ast-transformations:hoist-load-time-value ast))
	 (hir (cleavir-ast-to-hir:compile-toplevel hoisted)))
    
    (values hir hoisted)))

;;; function for defining specialisers
(defgeneric translate-instruction (instruction input-vars output-vars successor-branches)
  (:documentation "Converts a given instruction into some js-syntax.
SUCCESSOR-BRANCHES is a list of objects which represent statements
jumping to the corresponding successor -- one for each successor of instruction.
If the returned form has a single successor and does not refer to it then control implicitly
passes to the successor after the returned js-syntax executes.
INSTRUCTION should be a IR instruction.
INPUT-VARS is a list of symbols corresponding to the inputs of the instruction as JS variables.
OUTPUT-VARS is a list of symbols corresponding to the outputs of the instruction as JS variables.
If this returns a lisp object it is wrapped in a corresponding BRANCHING-JS object by translate.
If this returns a BRANCHING-JS object then it is not wrapped. The data is copied over."))

;;; some classes we use
(defclass branching-js ()
  ((js-syntax :initarg :js :accessor js)
   (successors :initarg :successors :accessor successors))
  ;; Note: successor order is not important once the instruction has been processed
  (:documentation "A wrapper for some JS syntax which knows where JS control should flow to after"))
(defclass implicitly-branching-js (branching-js) ()
  (:documentation "A wrapper for some JS syntax which forms a statement from which control should flow
automatically to the single successor"))
(defmethod (setf successors) :before (newvalue (arg implicitly-branching-js))
  (declare (ignore arg))
  (assert (= 1 (length newvalue)) () "Implicitly branching js may have only one successor"))
(defclass explicitly-branching-js (branching-js) ()
  (:documentation "A wrapper where JS control flows to one of the successors but the succeeding JS syntax should replace references to the successors in the source"))
(defclass caught-js-branch (explicitly-branching-js)
  ((catching-object :initarg :catching))
  (:documentation "A wrapper representing the entry point into a function after the stack unwinds"))
(defgeneric copy-in (from to)
  (:documentation "Copies slot values from FROM into TO")
  (:method ((from branching-js) (to branching-js))
    (if (slot-boundp from 'js-syntax)
	(setf (slot-value to 'js-syntax)
	      (slot-value from 'js-syntax))
	(slot-makunbound to 'js-syntax))
    (if (slot-boundp from 'successors)
	(setf (slot-value to 'successors)
	      (slot-value from 'successors))
	(slot-makunbound to 'successors))))
(defvar *branch-printer-inner* nil)
(defmethod print-object ((object branching-js) stream)
  (if *branch-printer-inner*
      (print-unreadable-object (object stream :identity t))
      (print-unreadable-object (object stream :type t :identity t)
	(let ((*branch-printer-inner* t))
	  (princ "SUCCESSORS: " stream)
	  (princ (successors object) stream)))))
(defmethod (setf js) :after (new old)
  (if (and (consp new) (eq (car new) 'js:if)) (break)))
;;; various special variabes used

(defvar *lexical-names*)
(defun var-symbol (lexical)
  "Returns the symbol for LEXICAL. If LEXICAL is a values-location, a list of two 
symbols are returned -- one for the first value and one for the rest of the values"
  (multiple-value-bind (val pres) (gethash lexical *lexical-names*)
    (if pres val
	(setf (gethash lexical *lexical-names*)
	      (typecase lexical
		(cleavir-ir:values-location
		 (list (gensym) (gensym)))
		(cleavir-ir:immediate-input
		 (if (numberp (cleavir-ir:value lexical))
		     `(js:number ,(cleavir-ir:value lexical))
		     (error "unrecognised immediate ~a" lexical)))
		(t
		 (gensym)))))))

(defvar *branching-wrappers*)
(defvar *wrapper-instructions*)
(defvar *branch-callers*)
(defun instruction-wrapper (instruction)
  (multiple-value-bind (val pres) (gethash instruction *branching-wrappers*)
    (if pres val
	(let ((new (make-instance 'branching-js)))
	  (setf (gethash instruction *branching-wrappers*) new)
	  (setf (gethash new *wrapper-instructions*) instruction)
	  (let ((sb (mapcar #'instruction-wrapper (cleavir-ir:successors instruction))))
	    (setf (successors new) sb))
	  new))))
(defun wrapper-instruction (branching-wrapper)
  (gethash branching-wrapper *wrapper-instructions*))

;;; some more special variables
(defvar *dynamic-scope-var*) ;holds a symbol for the variable holding the current dynamic scope (in js).
;;; dynamic scope implementation:
;;; All CL functions take dynamic scope as first arg
;;; some variable, say x, holds the current dynamic scope
;;; for some (CL) symbol s we can get value as:
;;; x[s.name].v and set it by x[s.name].v=foo
;;; to make a dynamic binding:
;;; x2 = Object.create(x); x2[s.name]={v:newfoo};
;;; We can't simply have x[s.name] for reading and x[s.name] = foo
;;; as the write updates only the top dynamic scope rather than the level
;;; where s was most recently bound.
(defvar *value-passing-var*) ;holds a symbol used to pass multiple values.
;;; multiple values are implemented as:
;;; (values a b c...) => var x = a; values-var = [b,c,...]; return x;
;;; 


;;; an anoying utility function
(defun map-tree-if (test fun tree)
  "for each atom leaf of tree, call test on it. If test succeeds then it is replaced by (fun leaf)
in the copy returned by this function. Otherwise it is left as-is"
  (if (atom tree)
      (if (funcall test tree)
	  (funcall fun tree)
	  tree)
      (loop for stree in tree
	   collect (map-tree-if test fun stree))))
(defun mapt (fun tree)
  "calls fun for each atom leaf of tree"
  (if (atom tree) (funcall fun tree)
      (loop for stree in tree do (mapt fun stree))))


;;; main entry point
(defun translate (instruction)
  (let ((wrap (instruction-wrapper instruction)))
    (if (slot-boundp wrap 'js-syntax)
	wrap
	(let ((inputs (cleavir-ir:inputs instruction))
	      (outputs (cleavir-ir:outputs instruction)))
	  (let ((input-vars (mapcar #'var-symbol inputs))
		(output-vars (mapcar #'var-symbol outputs))
		(successor-branches (successors wrap)))
	    (let ((r (translate-instruction instruction input-vars output-vars successor-branches)))
	      (if (typep r 'branching-js)
		  ;; we need to copy the data into WRAP
		  (progn
		    (change-class wrap (class-of r))
		    (copy-in r wrap))
		  ;;we need to wrap it
		  (if (= 1 (length successor-branches))
		      (let* ((b (first successor-branches))
			     (uses-successor
			      (block ret
				(mapt (lambda (x) (when (eq x b) (return-from ret t)))
				      r)
				nil)))
			(if uses-successor
			    (change-class wrap 'explicitly-branching-js
					  :js r)
			    (change-class wrap 'implicitly-branching-js
					  :js r)))
		      (change-class wrap 'explicitly-branching-js
				    :js r)))
	      ;; Now that we know what the JS successors of the instruction are, we record them.
	      (loop for s in (successors wrap)
		 do (pushnew wrap (gethash s *branch-callers* nil)))))
	  ;; finally return the wrapper
	  wrap))))

;;; Now the nitty-gritty
;;; What follows are methods on translate-instruction
;;; First up: ENTER-INSTRUCTION

;;; we need to first translate each instruction
;;; but in doing this we may encounter an unwind instruction in some deeper function
;;; so we need to record those too. we do this using signal
;;; This is the condition used to signal it
(define-condition unwind-found (condition)
  ((enter-instruction :initarg :invocation :accessor enter-instruction)
   (successors :initarg :successors :accessor successors)))
(defun unwind-found (enter-instruction successors)
  "signals that an unwind instruction was found which should unwind the stack to
the scope of ENTER-INSTRUCTION and then continue from the first of SUCCESSORS.
This returns a JS variable name that is used when calling js:throw to unwind to the right
place."
  (restart-bind ((unwind-identity (lambda (id)
				     (return-from unwind-found id))))
    (signal 'unwind-found
	    :invocation enter-instruction
	    :successors successors)))

(defun process-instructions (first &optional (callers-ht (make-hash-table)))
  "Translates all instructions in the tree starting at FIRST into JS.
FIRST should be an ENTER-INSTRUCTION
returns (values wrappers catch-wrappers callers-table entry-inst)
 where wrappers is a list of the wrappers for the JS to go into the body
 and catch-wrappers is a list of the wrappers that control should start from for various unwinds
 and callers-table maps wrappers to their predecessors
 and entry-inst is the block corresponding to the ENTER-INSTRUCTION.
If the CALLERS-HT arg is passed then it is used as the base for callers-table"
  (let ((*branching-wrappers* (make-hash-table))
	(*wrapper-instructions* (make-hash-table))
	(*branch-callers* callers-ht) ;HT mapping wrapper->predecessors
	)
    ;;we now translate the instructions via a breadth-first search
    (let* ((todo (list (instruction-wrapper (first (cleavir-ir:successors first)))))
	   (catchs ())
	   (done ()))
      (loop 
	 for inst = (pop todo)
	 while inst
	 do (let ((x (handler-bind
			 ((unwind-found (lambda (c)
					  (unless (eq first (enter-instruction c))
					    ;; we check that we should unwind to here
					    (return))
					  (assert (= 1 (length (successors c))) ()
						  "Unwinds must have only one successor")
					  (let* ((suc (first (successors c)))
						 (catch-var (gensym))
						 (cb (make-instance 'caught-js-branch
								    :successors (successors c)
								    :js `(js:block ,suc)
								    :catching catch-var)))
					    (push suc todo)
					    (push cb catchs)
					    (pushnew cb (gethash suc
							      callers-ht nil))
					    (invoke-restart 'unwind-identity catch-var)))))
		       (translate (wrapper-instruction inst)))))
	      (mapc (lambda (x) (unless (slot-boundp x 'js-syntax)
			     (push x todo)))
		    (successors x))
	      (push x done)))
      (values done catchs callers-ht (instruction-wrapper (first (cleavir-ir:successors first)))))))

(defun js-sequence (first second)
  "return a simple JS statement to do both first and second.
the idea is that (js-sequence '(js:block a b...) '(js:block c d...))
evaluates to '(js:block a b... c d...)"
  (flet ((block-parts (thing)
	   (cond
	     ((atom thing) (list thing))
	     ((and (consp thing) (eq (car thing) 'js:block))
	      (cdr thing))
	     ((equal thing '(js:empty)) nil)
	     (t (list thing)))))
    `(js:block ,@(block-parts first) ,@(block-parts second))))

(defun js-unblock (stuff)
  (if (and (consp stuff) (eq (car stuff) 'js:block))
      (cdr stuff)
      (list stuff)))

(defun js-subst (old new js &optional allow-many)
  "replaces exactly one instance of OLD in JS with NEW, without destructively modifying JS.
IF NEW is of the form (js:block ...) and OLD occurs in a block then it is spliced in.
OLD may not be at the head of a list"
  (let ((new-block (and (consp new) (member (car new) '(js:block js:empty))))
	(found 0))
    (labels ((process (list)
	       (let ((in-block (eq (car list) 'js:block)))
		 (loop for l in list
		    if (eq l old)
  		      do (incf found)
		      and if (and in-block new-block)
		        append (cdr new)
		      else collect new
		    else collect (if (consp l) (process l) l)))))
      (if (not (consp js))
	  new
	  (multiple-value-prog1 (values (process js) (= 1 found))
	    (unless allow-many
	      (case found
		(0 (warn "Failed to find any instances of ~a in ~a" old js))
		(1 nil)
		(t (error "Found multiple instances of ~a in ~a" old js)))))))))

(defun set-replace (old news list)
  "returns a list like LIST but with all references to OLD removed and (max 1 n)
 references to each of NEWS where n is the number of references to that element in LIST"
  (let ((found (loop for n in news collect nil)))
    (let ((n (loop for item in list
		unless (eq item old) collect item
		do (loop for c on found
		      for n in news
		      if (eq item n) do (setf (car c) t)))))
      (loop for x in news
	 for y on found
	 unless (car y) collect x
	 unless (cdr y) append n))))

(defgeneric merge-in (main suc)
  (:documentation "Given some successor of the block main, merge in the JS for successor
into the JS for main where the successor's place is either marked by a reference or implicit.")
  (:method :after ((main branching-js) (suc branching-js))
	   (setf (successors main)
		 (set-replace suc (successors suc) (successors main))))
  (:method :around ((main branching-js) (suc branching-js))
	   (let ((n (intersection (successors main) (successors suc))))
	     ;; we need to make sure that we don't have more than
	     ;; one reference to a successor in the merged JS
	     (case (length n)
	       (0 (call-next-method))
	       (1 (let ((next (first n)))
		    (call-next-method)
		    (let ((js (js main))
			  (name (gensym)))
		      (setf (js main)
			    `(js:block
				 (js:for nil nil nil
					 ,(js-subst next `(js:break ,name) js t)
					 ,name)
			       ,next)))))
	       (t (error "Too many common successors of ~a and ~a" main suc)))))
  (:method ((main implicitly-branching-js) (suc implicitly-branching-js))
    (setf (js main) (js-sequence (js main) (js suc))))
  (:method ((main explicitly-branching-js) (suc explicitly-branching-js))
    (setf (js main) (js-subst suc (js suc) (js main))))
  (:method ((main explicitly-branching-js) (suc implicitly-branching-js))
    (setf (js main) (js-subst suc (js-sequence (js suc) (first (successors suc)))
			      (js main))))
  (:method ((main implicitly-branching-js) (suc explicitly-branching-js))
    (change-class main 'explicitly-branching-js
		  :js (js-sequence (js main) (js suc)))))


(defun merge-one-predecessor (instructions callers-ht)
  "merges all instructions with only one predecessor into their predecessor.
returns (values new-instructions redo count)
where redo is T if there are instructions left with single predecessors
and count is the numbre of instructions that were merged.
new-instructions is a list of the set of instructions now left.
This function modifies callers-ht."
  (let ((update-map (make-hash-table))
	(count 0)
	(redo)
	(res))
    (labels ((updated (wrap)
	       (multiple-value-bind (val pres)
		   (gethash wrap update-map)
		 (if (and pres (not (eq val wrap)))
		     (multiple-value-bind (val2 pres2)
			 (gethash val update-map)
		       (if pres2
			   (let ((x (updated val2)))
			     (setf (gethash val update-map) x
				   (gethash wrap update-map) x))
			   val))
		     wrap)))
	     (update-to (old new)
	       (unless (eq old new)
		 (setf (gethash old update-map) new))
	       (remhash old callers-ht)
	       (incf count))
	     (predecessors (inst)
	       (multiple-value-bind (val pres) (gethash inst callers-ht)
		 (if pres val
		     (setf (gethash inst callers-ht nil) nil)))))
      (loop for inst in instructions
	   for p = (predecessors inst)
	 if (= 1 (length p))
	 do (merge-in (updated (first p)) inst)
 	    (update-to inst (first p)))
      (maphash (lambda (k v)
		 (push k res)
		 (setf redo
		       (or redo
			   (= 1 (length
				 (setf (gethash k callers-ht)
				       (remove-duplicates (mapcar #'updated v))))))))
	       callers-ht)
      (values res redo count))))

(defun really-merge-one-predecessor (instructions callers-ht)
  (prog ((count 0))
     (loop (multiple-value-bind (ins redo co)
	       (merge-one-predecessor instructions callers-ht)
	     (incf count co)
	     (if redo
		 (setf instructions ins)
		 (return-from really-merge-one-predecessor
		   (values ins count)))))))

(defun find-smallest-loopoid (instructions callers-ht)
  "finds the smallest set of instructions with only one external successor
and one external predecessor. Return (values blocks predecessor successor entry-point) for
the set of instructions"
  (let* ((ivec (apply #'vector instructions))
	 (rivec (let ((ht (make-hash-table))) (loop for i from 0 for s across ivec
						 do (setf (gethash s ht) i))
		     ht))
	 (predsuc (make-hash-table)))
    (labels ((predecessors (inst)
	       (gethash inst callers-ht))
	     (inst-num (inst)
	       (gethash inst rivec))
	     #|(make-iset (insts)
	       (let ((v (make-array (list (length ivec)) :element-type 'bit :initial-element 0)))
		 (loop for inst in insts do (setf (sbit v (inst-num inst)) 1))
		 v))
	     (map-iset (fun iset)
	       (loop for i across ivec for b of-type bit across iset if (= b 1) (funcall fun i)))
	     (iset-remove (iset to-remove)
	       (bit-andc2 iset to-remove t))|#

	     (make-iset (insts)
	       (let ((x 0))
		 (loop for inst in insts do (setf (ldb (byte 1 (inst-num inst)) x) 1))
		 x))
	     (map-iset (fun iset)
	       (loop for i across ivec for n from 0
		  if (logbitp n iset) do (funcall fun i n)))
	     (iset-union (&rest xs)
	       (apply #'logior xs))
	     (iset-remove (iset to-remove)
	       (logandc2 iset to-remove))
	     
	     (make-iset* (&rest insts) (make-iset insts))

	     (succ (iset)
	       (multiple-value-bind (val pres) (gethash iset predsuc)
		 (if pres val)
		 (let ((x 0))
		   (loop for i across ivec for n from 0
		      if (logbitp n iset) do (setf x (logior x (make-iset (successors i)))))
		   (setf (gethash iset predsuc) x))))
	     )
      (let ((current-sets
	     (let ((ht (make-hash-table :test 'eql)))
	       (loop for inst across ivec
		  for set = (make-iset* inst)
		  do (setf (gethash set ht) ;instructions in block
			   (cons
			    ;; we cons external successors/predecessors sets
			    (make-iset (successors inst))
			    (make-iset (predecessors inst)))))
	       ht))
	    (next-sets (make-hash-table))
	    (added))
	(macrolet ((add-set (blockvar newpre-form newsuc-form)
		     (let ((v1 (gensym)) (p1 (gensym))
			   (v2 (gensym)) (p2 (gensym))
			   (ns (gensym)) (np (gensym))
			   (p (gensym)) (s (gensym)) (ss (gensym)))
		       `(multiple-value-bind (,v1 ,p1) (gethash ,blockvar next-sets)
			  (declare (ignore ,v1))
			  (unless ,p1
			    (multiple-value-bind (,v2 ,p2) (gethash ,blockvar current-sets)
			      (if ,p2
				  (setf (gethash ,blockvar next-sets)
					,v2)
				  (let ((,np ,newpre-form) (,ns ,newsuc-form))
				    (let ((,p (iset-remove ,np ,blockvar)))
				      (when (<= 1 (logcount ,p)) ;1 extern. predecessor
					(let ((,s (iset-remove ,ns ,blockvar)))
					  (when (<= 1 (logcount ,s)) ;1 extern. successor
					    (let ((,ss (logand (succ ,p) ,blockvar)))
					      (when (<= 1 (logcount ,ss)) ;1 entry point
						(return-from processor
						  (values ,blockvar ,p ,s ,ss))))))))
				    (setf added t
					  (gethash ,blockvar next-sets)
					  (cons ,ns ,np))))))))))
	  (multiple-value-bind (block pre suc sta)
	      (block processor
		(loop
		   (loop for insts being the hash-keys of current-sets
		      using (hash-value (suc . pred))
		      do (map-iset
			  (lambda (s n)
			    (let* ((newblock (dpb 1 (byte 1 n) insts)))
			      (add-set newblock
				       (iset-union pred (make-iset (predecessors s)))
				       (iset-union suc newblock
						   (make-iset (successors s))))))
			  suc))
		   (rotatef next-sets current-sets)
		   (clrhash next-sets)
		   (unless added (return-from find-smallest-loopoid (values nil)))
		   (setf added nil)))
	    (let (b p s st)
	      (loop for i across ivec for n from 0
		 when (logbitp n block) do (push i b)
		 when (logbitp n pre) do (push i p)
		 when (logbitp n suc) do (push i s)
		 when (logbitp n sta) do (push i st))
	      (assert (null (cdr p)) () "Must have at most 1 predecessor")
	      (assert (null (cdr s)) () "Must have at most 1 successor")
	      (assert (null (cdr st)) () "Must have at most 1 entry point")
	      (values b (car p) (car s) (car st)))))))))

(defun divide (pred list &key (return :both))
  "Split list based on the results of pred. RETURN can be one of :BOTH :TRUE :FALSE
:BOTH returns (values true false)"
  (ecase return
    (:both
     (loop for x in list
	if (funcall pred x)
	  collect x into true
	else
	  collect x into false
	finally (return (values true false))))
    (:true (remove-if-not pred list))
    (:false (remove-if pred list))))

(defun js-from-blocks (blocks entry-point)
  "returns some JS to do whatever is decribed in blocks.
JS control starts flowing through ENTRY-POINT which should
be a member of BLOCKS which is a list of explicitly-branching-js blocks"
  (declare (optimize (debug 3)))
  (let* ((bvec (apply #'vector blocks))
	 (nblocks (length bvec))
	 (rbvec (make-hash-table))
	 (paths (make-array (list nblocks nblocks)
			    :element-type 'boolean
			    :initial-element nil)))
    (loop for i from 0 for b across bvec do (setf (gethash b rbvec) i))
    ;; we populate paths so that (aref paths a b) <=> a path from a to b exists
    (loop for i from 0 for b across bvec do (loop for s in (successors b)
					       for num = (gethash s rbvec)
					       when num do (setf (aref paths i num) t)))
    (let ((svec (make-array
		 (list nblocks)
		 :element-type '(simple-array fixnum (*))
		 :initial-contents
		 (loop for b across bvec
		    collect (loop for s in (successors b)
			       for num = (gethash s rbvec)
			       when num collect it into nums and count t into n
			       finally (return (sort
						(make-array (list n) :element-type 'fixnum
							    :initial-contents nums)
						#'<)))))))
      (loop while
	   (plusp
	    (loop for i from 0 below nblocks
	       for b across bvec
	       for s across svec
	       sum (loop for j from 0 below nblocks
		      count (unless (aref paths i j)
			      (setf (aref paths i j)
				    (loop for k across s
				       thereis (and (aref paths i k)
						    (aref paths k j))))))))))
    ;; now we need to add loops and breaks/continues to do control flow
    (let (context)
      (declare (special context))
      (labels
	  ((block-num (block)
	     (gethash block rbvec))
	   (make-block-bvec (&rest blocks)
	     (let ((r (make-array (list (length bvec))
				  :element-type 'bit
				  :initial-element 0)))
	       (loop for b in blocks do (setf (sbit r (block-num b)) 1))
	       r))
	   (path-exists (from to)
	     (aref paths (block-num from) (block-num to)))
	   (path-exists-in-subset (subset from to)
	     (and (path-exists from to)
		  (let ((e (if (and (consp context) (consp (car context))
				    (eq (caar context) 'loop))
			       (cdar context))))
		    (if (member to e) (return-from path-exists-in-subset nil))
		    (loop with seen = (make-block-bvec)
		       for edge = (successors from)
		       then 
			 (remove-duplicates
			  (loop for s in edge
			     when (eq s to) do (return-from path-exists-in-subset t)
			     when (and (under-consideration s)
				       (not (member s e))
				       (member s subset)
				       (path-exists s to)
				       (not (= 1 (aref seen (block-num s)))))
			     append (successors s)
			     and do (setf (aref seen (block-num s)) 1)))
		       always edge))))
	   (under-consideration (block)
	     (nth-value 1 (gethash block rbvec)))
	   
	   
	   (simple (block next)
	     (list 'simple block next))
	   (loop-block (inner next)
	      (list 'loop inner next))
	   (multiple (handled next)
	     (list 'multiple handled next))

	   (reloop-make-loop (blocks entry-points)
	     (multiple-value-bind (inner outer)
		 (divide
		  (lambda (x) (loop for e in entry-points
			    thereis (path-exists-in-subset blocks x e)))
		  blocks)
	       (loop-block
		  (let ((context (cons (cons 'loop entry-points) context)))
		    (declare (special context))
		    (reloop inner
			    entry-points))
		  (reloop outer
			  (divide
			   (lambda (x) (loop for it in inner
				     thereis (member x (successors it))))
			   outer
			   :return :true)))))
	   
	   (reloop (blocks entry-points)
	     (when (null entry-points)
	       (cerror "return NIL" "no entry points. blocks=~a" blocks)
	       (return-from reloop nil))
	     (when (null blocks) (break))
	     (cond
	       ((and (= 1 (length entry-points))
		     (not (path-exists-in-subset
			   blocks
			   (first entry-points) (first entry-points))))
		(simple (first entry-points)
			(let* ((next-blocks (remove (first entry-points) blocks))
			       (ss (divide
				    (lambda (x) (member x next-blocks))
				    (successors (first entry-points))
				    :return :true)))
			  (and ss
			       (reloop next-blocks
				       ss)))))
	       
	       ((loop for e in entry-points always (loop for f in entry-points
						      always (path-exists-in-subset blocks e f)))
		(reloop-make-loop blocks entry-points))

	       ((< 1 (length entry-points))
		(let*
		    ((any))
		  (multiple-value-bind
			(mapping all)
		      (loop for e in entry-points
			 for x =	;for each entry-point
			   (loop for b in blocks ;get a list of blocks
			      when (and
				    (path-exists-in-subset blocks e b) ;accesible by that enrtry point
				    (loop named unique ;and only that entry point
				       for f in entry-points
				       when (and (not (eq e f)) (path-exists-in-subset blocks f b))
				       do (return-from unique nil)
				       finally (return-from unique t)))
			      collect b)
			 when x do (setf any t)
			 collect (cons e x) into mapping
			 append x into all
			 finally (return (values mapping all)))
		    (break)
		    (if (not any)
			(reloop-make-loop blocks entry-points)
			(let ((all (remove-duplicates all)))
			  (multiple-value-bind (cond next)
			      (divide #'cdr mapping)
			    (multiple
			     (let ((context (cons 'handled context)))
			       (declare (special context))
			       (loop for (e . r) in cond
				  collect (reloop r (list e))))
			     (let ((rest (set-difference blocks all)))
			       (reloop rest
				       (union
					(mapcar #'car next)
					(loop for x in all
					   append (intersection (successors x)
								rest))))))))))))
	       (t (reloop-make-loop blocks entry-points)))))
	(reloop blocks (list entry-point))
	))))

;;; We implement the relooper algorithm from emscripten to sort out the rest of the control flow
(defvar *relooper-context* ())
(defvar *relooper-alist* ())
(defvar *relooper-label* (gensym))
(defvar *relooper-label-counter* 0)

(defun make-reverse-block-lookup-ht (vec)
  (let ((ht (make-hash-table :test 'eq)))
    (loop for x across vec for i from 0
       do (setf (gethash x ht) i))
    ht))

(declaim (inline block-num))
(defun block-num (block lookup) (if (integerp block) block (gethash block lookup)))

(defun block-accessible-p (block)
  (not (and (consp *relooper-context*)
	    (consp (car *relooper-context*))
	    (eq 'loop (caar *relooper-context*))
	    (member block (cdar *relooper-context*)))))

(defun make-successors-array (block-vec block-lookup-ht &aux (n (length block-vec)))
  (make-array (list n) :element-type '(simple-array fixnum (*))
	      :initial-contents
	      (loop for block across block-vec for index from 0
		 for x = (loop for s in (successors block)
			    for n = (block-num s block-lookup-ht)
			    when (and n
				      (block-accessible-p s))
			    collect n)
		 collect (make-array (list (length x)) :element-type 'fixnum
				     :initial-contents x))))

(defun block-accessible (block-vec successors-array)
  (declare (optimize (debug 3)))
  (let ((n (length block-vec)))
    (let ((accessibility-matrix (make-array (list n n) :element-type 'boolean :initial-element nil)))
      	  ;(aref accessibility-matrix i j) <=> (path-exists block_i block_j)
      ;;first step initialisation of accessibility matrix
      (loop for block across block-vec
	 for index from 0
	 do (loop for suc across (aref successors-array index)
	       do (setf (aref accessibility-matrix index suc) t)))
      ;; now we iterate to fill out the rest of the elements
      ;; we want to be accessing accessibility-matrix in row-major-order if possible
      ;; i.e. changing the second index more than the first
      (loop for c = 1 then
	   (loop
	      for i from 0 below n
	      sum (loop
		     for k from 0 below n
		     if (not (aref accessibility-matrix i k))
		     sum (loop
			    for j from 0 below n
			    if (and (aref accessibility-matrix i j) (aref accessibility-matrix j k))
			    count (setf (aref accessibility-matrix i k) t))))
	 until (zerop c))
      accessibility-matrix)))

(defun relooper-fresh-label ()
  (incf *relooper-label-counter*))

(defun relooper-alist-js (cdr)
  (destructuring-bind (label breaker) cdr
    (if (null label)
	(if (null breaker)
	    '(js:empty)
	    breaker)
	(if (null breaker)
	    `(js:= ,*relooper-label* (js:number ,label))
	    `(js:block
		 (js:= ,*relooper-label* (js:number ,label))
	       ,breaker)))))

(defun relooper-subst (block)
  (loop for s in (successors block)
     for a = (assoc s *relooper-alist*)
     when a
     do (setf (js block)
	      (js-subst s (relooper-alist-js (cdr a)) (js block))
	      (successors block)
	      (set-replace s (successors s) (successors block)))))

(defun relooper-do-loop (blocks bvec rbvec entry-points envec svec accessible)
  (declare (optimize (debug 3)))
  (macrolet ((accessible (from to) `(aref accessible (block-num ,from rbvec) (block-num ,to rbvec))))
    (multiple-value-bind (inner outer)
	(divide (lambda (block)
		  (loop for e across envec thereis (accessible block e)))
		blocks)
      ;; inner blocks can get to some entry point
      (let* ((nextentries      ;directly accessible outers from inners
	      (let ((ne ()))
		(loop for b in inner
		   for n = (block-num b rbvec)
		   do (loop for s across (aref svec n)
			 for sb = (aref bvec s)
			 when (member sb outer)
			 do (pushnew sb ne)))
		ne))
	     (label (gensym))
	     (inner-branches (loop for e in entry-points collect (list nil `(js:continue ,label)))))
	(if outer
	    (multiple-value-bind (block ep)
		(relooper outer nextentries)
	      (let ((*relooper-alist*
		     (pairlis entry-points inner-branches
		      (pairlis nextentries
			       (mapcar (lambda (x) (list (first x) `(js:break ,label))) ep)
			       *relooper-alist*)))
		    (*relooper-context* (cons (cons 'loop entry-points) *relooper-context*)))
		(multiple-value-bind (inner-block ep)
		    (relooper inner entry-points)
		  (setf (js inner-block)
			(js-sequence
			 `(js:for nil nil nil ,(js inner-block) ,label)
			 (js block))
			(successors inner-block)
			(successors block))
		  (values inner-block
			  (loop for (lab nil) in ep collect (list lab nil))))))
	    (let ((*relooper-alist*
		   (pairlis entry-points inner-branches
			    *relooper-alist*))
		  (*relooper-context* (cons (cons 'loop entry-points) *relooper-context*)))
	      (multiple-value-bind (inner-block ep)
		  (relooper inner entry-points)
		(setf (js inner-block)
		      `(js:for nil nil nil ,(js inner-block) ,label))
		(values inner-block
			(loop for (lab nil) in ep collect (list lab nil))))))))))

(defun relooper (blocks entry-points)
  "Returns (values block entry-lists)
where block is a block of js-code to run and
entry-lists is a list of the same length as entry points where
each element represents the js to get to that entry point in the
form of a list: (label breaker) where label is an integer to set a 
label variable to (or NIL if not needed) and breaker is the control-flow
statement to get to entry-point -- either NIL, (js:break thing) or (js:continue thing)."
  (declare (optimize (debug 3)))
  (let* ((bvec (apply #'vector blocks))
	 (rbvec (make-reverse-block-lookup-ht bvec))
	 (envec (map '(vector fixnum) (lambda (e) (block-num e rbvec)) entry-points))
	 (svec (make-successors-array bvec rbvec))
	 (accessible (block-accessible bvec svec))
	 (nentries (length entry-points)))
    (when (null blocks)
      (cerror "Return NIL" "blocks=NIL. entry-points=~a" entry-points)
      (return-from relooper nil))
    (macrolet ((accessible (from to) `(aref accessible (block-num ,from rbvec) (block-num ,to rbvec))))
      (cond
	((and (= 1 nentries) (not (accessible (first entry-points) (first entry-points))))
	 ;; make a simple block
	 (let* ((first (first entry-points))
		(rest (remove first blocks))
		(next (map 'list (lambda (x) (aref bvec x)) (aref svec (block-num first rbvec)))))
	   (if next
	       (multiple-value-bind (block ep) (relooper rest next)
		 (let ((*relooper-alist* (pairlis entry-points ep *relooper-alist*)))
		   (relooper-subst first))
		 (setf (js first) (js-sequence (js first) (js block)))
		 (values first (list (list nil nil))))
	       (progn
		 (relooper-subst first)
		 (values first (list (list nil nil)))))))
	((loop for e1 across envec
	    always (loop for e2 across envec
		      #|maybe thereis|# always (accessible e1 e2)))
	 ;; we can get from any entry to any other entry
	 ;; so make a loop
	 (relooper-do-loop blocks bvec rbvec entry-points envec svec accessible))
	((> nentries 1)
	 ;; we try to make a multiple block
	 (let* ((should-multiple nil)
		(all)
		(mapping
		 (loop for e across envec
		    for r =
		      (loop for b across bvec
			 when (and (accessible e b)
				   (loop for e2 across envec
				      never (unless (eq e e2)
					      (accessible e2 b))))
			 collect b)
		    when r do (setf should-multiple t)
		    collect (cons (aref bvec e) r)
		    append r into a
		    finally (setf all a))))
	   (if (not should-multiple)
	       (relooper-do-loop blocks bvec rbvec entry-points envec svec accessible)
	       (multiple-value-bind
		     (handled others) (divide #'cdr mapping)
		 (let* ((next-blocks (set-difference blocks all))
			(next-entries (loop for inner in all
					 append (loop for sn across (aref svec (block-num inner rbvec))
						   for s = (aref bvec sn)
						   when (member s next-blocks)
						   collect s)))
			(next-entries (union next-entries (mapcar #'car others)))
			(next-blocks (union next-blocks next-entries)))
		   (multiple-value-bind (next ep)
		       (if next-blocks (relooper next-blocks next-entries)
			   (values nil nil))
		     (let ((label (gensym)))
		       (let* ((*relooper-context* (cons 'multiple *relooper-context*))
			      (*relooper-alist* (pairlis next-entries
							 (loop for (l nil) in ep
							    collect (list l `(js:break ,label)))
							 *relooper-alist*))
			      (nums (loop for x in handled collect (relooper-fresh-label)))
			      (*relooper-alist*
			       (pairlis
				(mapcar #'car handled)
				(loop for n in nums
				   for (e) in handled
				   collect (list n (let ((a (assoc e *relooper-alist*)))
						     (when a (second (cdr a))))))
				*relooper-alist*)))
			 (let ((inners (loop for (e . b) in handled
					  collect (relooper (union b (list e)) (list e))
					  ;; we can ignore the entry stuff as this has a single
					  ;; entry point so generates a block or a loop, both of
					  ;; which start at their entry point
					    )))
			   (let ((js
				  `(js:switch (,*relooper-label* ,label)
					      ,@(loop for ib in inners
						   for n in nums
						   collect `(js:cases (,n)
								      ,@(js-unblock (js ib))))
					      (js:default))))
			     (values
			      (make-instance 'explicitly-branching-js
					     :js (js-sequence js (if next (js next)
								     '(js:empty)))
					     :successors () ;???
					     )
			      (loop for e in entry-points
				 for x = (assoc e handled)
				 if x collect (list (first (cdr (assoc e *relooper-alist*))) nil)
				 else collect (cdr (assoc e (pairlis next-entries ep)))))))))))))))
	(t (relooper-do-loop blocks bvec rbvec entry-points envec svec accessible))))))


(defmethod translate-instruction ((inst cleavir-ir:enter-instruction) in out suc)
  ;; We need to take the successors of inst and make some JS syntax
  (multiple-value-bind (all catches callers-ht)
      (process-instructions inst)
    (setf all (really-merge-one-predecessor all callers-ht))
    )
  )


;;; Some standard instructions

(defmethod translate-instruction ((inst cleavir-ir:assignment-instruction) in out suc)
  (declare (ignore inst suc))
  `(js:= ,@out ,@in))

(defmethod translate-instruction ((inst cleavir-ir:nop-instruction) in out suc)
  (declare (ignore in out suc))
  '(js:empty))

(defmethod translate-instruction ((inst cleavir-ir:fdefinition-instruction) in out suc)
  (declare (ignore inst suc))
  ;; TODO: if we know what the function is then just write its name explicitly each time it's called
  ;; (first in) is a symbol.
  `(js:= ,@out (js:\. ,@in ,+symbol-function-tag+)))


;;; some multiple-value related instructions

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
		     defining))
	 (and (every (lambda (u)
		       (and (typep u 'cleavir-ir:multiple-to-fixed-instruction)
			    (= 1 (length (cleavir-ir:outputs u)))))
		     using))))))

(defmethod translate-instruction ((inst cleavir-ir:funcall-instruction) in out suc)
  (declare (ignore suc))
  (if (need-both-values (first (cleavir-ir:outputs inst)))
      `(js:\, (js:= ,(caar out) (js:call ,(first in) ,*dynamic-scope-var* ,@(rest in)))
	      (js:= ,(cadar out) ,*value-passing-var*))
      `(js:= ,(caar out) (js:call ,(first in) ,*dynamic-scope-var* ,@(rest in)))))

(defmethod translate-instruction ((inst cleavir-ir:multiple-value-call-instruction) in out suc)
  (declare (ignore suc))
  (let ((call `(js:call (js:\. ,(first in) "apply")
			js:null
			(js:call (js:\. (js:vector ,*dynamic-scope-var*) "concat")
				 ,@(loop for (f r) in (rest in)
				      collect `(js:vector ,f)
				      collect r)))))
    ;; (mvcall foo (a...) (b...) (c...)) where output of (a...) is (values a . aa), etc
    ;; => foo.apply(null, [scope].concat([a],aa,[b],bb,[c],cc);
    (if (need-both-values (first (cleavir-ir:outputs inst)))
	`(js:\, (js:= ,(caar out) ,call)
		(js:= ,(cadar out) ,*value-passing-var*))
	`(js:= ,(caar out) ,call))))

(defmethod translate-instruction ((inst cleavir-ir:return-instruction) in out suc)
  (declare (ignore suc))
  (if (need-both-values (first (cleavir-ir:inputs inst)))
      `(js:block
	   (js:= ,*value-passing-var* ,(cadar in))
	 (js:return ,(caar in)))
      `(js:return ,(caar in))))

(defmethod translate-instruction ((inst cleavir-ir:multiple-to-fixed-instruction) in out suc)
  (declare (ignore suc))
  (case (length out)
    (0 '(js:empty))
    (1 `(js:= ,(car out) ,(caar in)))
    (t `(js:\, (js:= ,(car out) ,(caar in))
	       ,@(loop for o in (cdr out)
		      for i from 0
		    collect `(js:= ,o (js:? (js:in (js:number ,i) ,*value-passing-var*)
					    (js:\. ,*value-passing-var* (js:number ,i)))))))))

(defmethod translate-instruction ((inst cleavir-ir:fixed-to-multiple-instruction) in out suc)
  (let ((loc (first (cleavir-ir:outputs inst))))
    (if (or (< 1 (length in))
	    (need-both-values (first (cleavir-ir:outputs inst)))
	    (some (lambda (u) (typep u 'cleavir-ir:return-instruction))
		  (cleavir-ir:using-instructions loc)))
	`(js:\, (js:= ,(caar out) ,(car in))
		(js:= ,(cadar out) (js:vector ,@(cdr in))))
	`(js:= ,(caar out) ,(car in)))))


;;; Some branching instructions

(defun make-if (test suc)
  `(js:if ,test ,@suc))

(defmethod translate-instruction ((inst cleavir-ir:eq-instruction) in out suc)
  (declare (ignore out))
  (make-if `(js:=== ,@in) suc))

