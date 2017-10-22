(defpackage #:js-syntax-writer
  (:use #:cl)
  (:export #:write-js-to-stream #:write-js-to-string))
(defpackage #:js-syntax
  (:nicknames #:js)
  (:export #:block
	   #:var
	   #:global
	   #:empty
	   #:if
	   #:do-while
	   #:while
	   #:for
	   #:for-in
	   #:break
	   #:continue
	   #:return
	   #:switch
	   #:case
	   #:cases
	   #:default
	   #:throw
	   #:try
	   #:catch
	   #:function
	   #:named-function

	   #:null			;TODO
	   #:undefined			;TODO

	   #:this

	   #:true
	   #:false
	   
	   #:number
	   #:string
	   #:var

	   #:vector
	   #:object			;TODO

	   ;; precedence 18:
	   #:new ;(new with arguments e.g. new Foo())
	   #:\. ;member access
	   
	   ;; precedence 17:
	   #:call

	   ;;precedence 16:
	   ;(new without arguments e.g. new Foo)
	   
	   ;;precedence 15:
	   #:post++
	   #:post--
	   
	   ;; precedence 14:
	   ;unary -, unary +
	   #:!
	   #:~ ;(bitwise not)
	   #:pre++
	   #:pre--
	   #:typeof
	   #:void
	   #:delete

	   #:*				;precedence 13
	   #:/				;precedence 13
	   #:%				;precedence 13
	   #:-				;precedence 12
	   #:+				;precedence 11
	   ;; these have precedence 10
	   #:<<
	   #:>>
	   #:>>>
	   ;; these have precedence 9
	   #:<
	   #:>
	   #:<=
	   #:>=
	   #:instanceof
	   #:in
	   ;; these have precedence 8
	   #:==
	   #:!=
	   #:===
	   #:!==

	   #:&			       ;precedence 7
	   #:^ 				;precedence 6
	   #:\| 			;precedence 5
	   #:&& 			;precedence 4
	   #:\|\|		       ;precedence 3
	   #:?				;precedence 2
	   ;; all below have precedence 1:
	   #:=
	   #:*= #:/= #:%=
	   #:+= #:-=
	   #:<<= #:>>= #:>>>=
	   #:&= #:^= #:\|=

	   #:\,				;precedence 0
	   ))

;;; TODO: most expressions

(in-package #:js-syntax-writer)

(defvar *js-stream*)
(defvar *js-statement*) ;are we able to write a statement?
(defvar *js-statement-written*) ;have we written a statement (or an expression)
(defvar *js-expression-level*) ;based on https://developer.mozilla.org/en/docs/Web/JavaScript/Reference/Operators/Operator_Precedence except we number expressions based on the right column rather than the left.
;;; the above holds the precedence level that an expression must be to not need brackets
(defvar *js-can-have-brackets*)

(defvar *js-function-vars*) ;this is a has-table mapping symbols to strings
(defvar *js-var-counter*) ;this is a counter used to ensure we get unique vars
;;; these used to be an alist and number that were let to themselves for the body of each
;;; function. Thus, variables names would be function local. The porblem is as that the following is
;;; valid but might not compile properly:
;;; ((js:function foo (x) (js:if x (js:call bar) (js:number 1)))
;;;  (js:function bar (x) (js:if x (js:call foo) (js:number 0))))

(defun write-js-to-stream (program stream &key (whole t))
  (let ((*js-stream* stream))
    (if whole
	(write-js-whole-program program)
	(write-js-program program))))
(defun write-js-to-string (program &key (whole t))
  (with-output-to-string (*js-stream*)
    (if whole
	(write-js-whole-program program)
	(write-js-program program))))

(defun write-js-whole-program (statements)
  (let ((*js-function-vars* (make-hash-table))
	(*js-var-counter* 0))
    (write-js-program statements)))

(defun write-js-program (statements)
  (loop for statement in statements do (write-js-statement statement)))
(defun write-js-statement (statement)
  (let ((*js-statement* t)
	(*js-statement-written* nil)
	(*js-expression-level* 0)
	(*js-can-have-brackets* t))
    (write-js-thing statement)
    (unless *js-statement-written* (write-js-stream ";")))) ;we put a semicolon after expressions
(defun write-js-expression (expression &key
					 (level *js-expression-level*)
					 (can-have-brackets t))
  (let ((*js-statement* nil)
	(*js-expression-level* level)
	(*js-can-have-brackets* can-have-brackets))
    (write-js-thing expression)))

(defun write-js-thing (thing)
  (typecase thing
    (cons (write-js-syntax (car thing) (cdr thing)))
    ((member t nil js-syntax:true js-syntax:false)
     (case thing
       ((t js-syntax:true)
	(write-js-syntax 'js-syntax:true nil))
       ((nil js-syntax:false)
	(write-js-syntax 'js-syntax:false nil))))
    (symbol
     (case thing
       (js-syntax:null (write-js-syntax 'js-syntax:null nil))
       (js-syntax:this (write-js-syntax 'js-syntax:this nil))
       (js-syntax:undefined (write-js-syntax 'js-syntax:undefined nil))
       (t (write-js-var thing))))
    (number (write-js-syntax 'js-syntax:number (list thing)))
    (string (write-js-syntax 'js-syntax:string (list thing)))
    (t (error "Not JS syntax: ~a" thing))))

(defun is-valid-js-identifier (thing)
  (and (stringp thing)
       (not (member
	     thing
	     '("" "break" "case" "catch" "class" "const" "continue"
	       "debugger" "default" "delete" "do" "else" "enum" "export"
	       "extends" "false" "finally" "for" "function" "if"
	       "implements" "import" "in" "interface" "instanceof" "let"
	       "new" "null" "package" "private" "protected" "public"
	       "return" "static" "super" "switch" "this" "throw" "true"
	       "try" "typeof" "var" "void" "while" "with" "yield")
	     :test #'string=)) ;reserved words; some only reserved in strict mode
       (not (member
	     thing
	     '("Infinity" "NaN" "undefined")
	     :test #'string=)) ;not reserved words but behave weirdly
       (not (member
	     (char thing 0)
	     '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9
	       #\{ #\} #\( #\) #\< #\> #\[ #\]
	       #\= #\+ #\- #\* #\/
	       #\\ ;technically identifiers can contain \u... like in java but this
		   ;will soon be removed from the spec
	       #\? #\~ #\| #\` #\^
	       #\Nul #\Soh #\Stx #\Etx #\Eot #\Enq #\Ack
	       #\Bel #\Backspace #\Tab #\Newline #\Vt
	       #\Page #\Return #\So #\Si #\Dle #\Dc1 #\Dc2
	       #\Dc3 #\Dc4 #\Nak #\Syn #\Etb #\Can #\Em
	       #\Sub #\Esc #\Fs #\Gs #\Rs #\Us #\Space
	       #\! #\" #\# #\% #\& #\' #\, #\. #\: #\;
	       #\@ #\Del)
	     :test #'char=))
       (notany (lambda (char)
		 (member
		  char
		  '(#\{ #\} #\( #\) #\< #\> #\[ #\]
		    #\= #\+ #\- #\* #\/ #\\ #\? #\~ #\| #\` #\^
		    #\Nul #\Soh #\Stx #\Etx #\Eot #\Enq #\Ack
		    #\Bel #\Backspace #\Tab #\Newline #\Vt
		    #\Page #\Return #\So #\Si #\Dle #\Dc1 #\Dc2
		    #\Dc3 #\Dc4 #\Nak #\Syn #\Etb #\Can #\Em
		    #\Sub #\Esc #\Fs #\Gs #\Rs #\Us #\Space
		    #\! #\" #\# #\% #\& #\' #\, #\. #\: #\;
		    #\@ #\Del)
		  :test #'char=))
	       thing)
       #+sb-unicode (every
		     (lambda (char) (< (char-code char) #.(expt 2 16))) ;JS strings are UCS16
		     thing)
       #+sb-unicode (or
		     (member (char thing 0) '(#\$ #\_) :test #'char=)
		     (member (sb-unicode:general-category (char thing 0))
			     '(:lu :ll :lt :lm :lo :nl)))
       #+sb-unicode (every
		     (lambda (char)
		       (or (char= char #\$)
			   (char= char #\_)
			   (char= char #\ZERO_WIDTH_NON-JOINER)
			   (char= char #\ZERO_WIDTH_JOINER)
			   (member (sb-unicode:general-category char)
				   '(:lu :ll :lt :lm :lo :nl
				     :mn :mc :nd :pc))))
		     thing)))

(defun write-js-identifier (id &key (check t))
  (when check (assert (is-valid-js-identifier id)))
  (write-js-stream id))

(defun get-js-var-string (symbol)
  (or (gethash symbol *js-function-vars*)
       (setf (gethash symbol *js-function-vars*) (make-new-var))))

(defun write-js-var (symbol)
  (write-js-identifier
   (get-js-var-string symbol)
   :check nil))

(defun write-js-label (thing)
  (if (symbolp thing)
      (write-js-var thing)
      (write-js-identifier thing)))

(defun write-js-stream (string)
  (write-string string *js-stream*))

(defgeneric write-js-syntax (car cdr))

(defmacro def-js-syntax (name lambda-list &body body)
  (let ((car (gensym (symbol-name name)))
	(cdr (gensym "CDR")))
    `(defmethod write-js-syntax ((,car (eql ',name)) ,cdr)
       (declare (ignore ,car))
       (destructuring-bind ,lambda-list ,cdr
	 ,@body))))

(defmacro asserting-js-statement (&body body)
  `(if *js-statement*
       (progn ,@body (setf *js-statement-written* t))
       (error "Attempt to put statement in expression")))

(defmacro in-js-statement (&body body)
  `(asserting-js-statement
     ,@body
     (write-js-stream ";")))

(defmacro at-expression-level ((level &optional sublevel) &body body)
  (let ((exp (gensym "EXPRESSION")))
    (if sublevel
	`(let* ((,exp (> *js-expression-level* ,level))
		(*js-expression-level* ,sublevel))
	   (if ,exp (write-js-stream "("))
	   ,@body
	   (if ,exp (write-js-stream ")")))
	`(flet ((,exp () ,@body))
	   (if (> *js-expression-level* ,level)
	       (if *js-can-have-brackets*
		   (let ((*js-expression-level* 0))
		     (write-js-stream "(")
		     (,exp)
		     (write-js-stream ")"))
		   (error "Cannot put lower precedence operator in high-precedence place"))
	       (,exp))))))

(def-js-syntax js-syntax:block (&rest statements)
  (asserting-js-statement
    (write-js-stream "{")
    (write-js-program statements)
    (write-js-stream "}")))

(defun make-new-var ()
  (format nil "l~36r" (incf *js-var-counter*)))

(def-js-syntax js-syntax:var (&rest bindings)
  (in-js-statement
    (write-js-stream "var")
    (loop for b in bindings
       for first = t then nil
       if (and (consp b) (cdr b))
       do (write-js-stream (if first " " ","))
	 (write-js-var (car b))
	 (write-js-stream "=")
	 (write-js-expression (second b))
       else if (consp b)
       do (write-js-stream " ")
	 (write-js-var (car b))
       else do (write-js-stream (if first " " ","))
	 (write-js-var b))))

(def-js-syntax js-syntax:global (name)
  (write-js-identifier name))

(def-js-syntax js-syntax:empty () (in-js-statement))

(def-js-syntax js-syntax:if (test then &optional else)
  (asserting-js-statement
    (flet ((if-no-elsep (x)
	     (and (consp x)
		  (eq 'js-syntax:if (car x))
		  (not (fourth x)))))
      (write-js-stream "if(")
      (write-js-expression test)
      (write-js-stream ")")
      (if else
	  (progn
	    (if (if-no-elsep then)
		(write-js-statement `(js-syntax:block ,then))
		(write-js-statement then))
	    (write-js-stream "else ")
	    (write-js-statement else))
	  (write-js-statement then)))))

(def-js-syntax js-syntax:do-while (statement test &optional name)
  (asserting-js-statement
    (when name
      (write-js-label name)
      (write-js-stream ":"))
    (write-js-stream "do ")
    (write-js-statement statement)
    (write-js-stream "while(")
    (write-js-expression test)
    (write-js-stream ");")))

(def-js-syntax js-syntax:while (test statement &optional name)
  (asserting-js-statement
    (when name
      (write-js-label name)
      (write-js-stream ":"))
    (write-js-stream "while(")
    (write-js-expression test)
    (write-js-stream ")")
    (write-js-statement statement)))

(def-js-syntax js-syntax:for (pre test iter statement &optional name)
  (asserting-js-statement
    (when name
      (write-js-label name)
      (write-js-stream ":"))
    (write-js-stream "for(")
    (if pre
	(if (and (consp pre) (eq (car pre) 'js-syntax:var))
	    (write-js-statement pre)
	    (progn (write-js-expression pre) (write-js-stream ";")))
	(write-js-stream ";"))
    (when test (write-js-expression test))
    (write-js-stream ";")
    (when iter (write-js-expression iter))
    (write-js-stream ")")
    (write-js-statement statement)))

(def-js-syntax js-syntax:for-in (var-p var in statement &optional name)
  (asserting-js-statement
    (when name
      (write-js-label name)
      (write-js-stream ":"))
    (write-js-stream "for(")
    (when var-p (write-js-stream "var "))
    (write-js-expression var)
    (write-js-stream " in ")
    (write-js-expression in)
    (write-js-stream ")")
    (write-js-statement statement)))

(def-js-syntax js-syntax:continue (&optional name)
  (in-js-statement
    (if name
	(progn
	  (write-js-stream "continue ")
	  (write-js-label name))
	(write-js-stream "continue"))))

(def-js-syntax js-syntax:break (&optional name)
  (in-js-statement
    (if name
	(progn
	  (write-js-stream "break ")
	  (write-js-label name))
	(write-js-stream "break"))))

(def-js-syntax js-syntax:return (&optional value)
  (in-js-statement
    (if value
	(progn
	  (write-js-stream "return ")
	  (write-js-expression value))
	(write-js-stream "return"))))

(defvar *js-switch-name*)
(defvar *js-switch-had-default*)

(def-js-syntax js-syntax:switch ((on &optional name) &rest body)
  (asserting-js-statement
    (when name
      (write-js-identifier name)
      (write-js-stream ":"))
    (write-js-stream "switch(")
    (write-js-expression on)
    (write-js-stream ")")
    (write-js-stream "{")
    (let ((*js-switch-name* name)
	  (*js-switch-had-default* nil))
      (write-js-program body))
    (write-js-stream "}")))

(def-js-syntax js-syntax:case (thing)
  (asserting-js-statement
    (if (eq thing 'js-syntax:default)
	(progn
	  (write-js-stream "default")
	  (when *js-switch-had-default*
	    (warn "Generating javascript switch(){} with multiple default: clauses"))
	  (setf *js-switch-had-default* t))
	(progn
	  (write-js-stream "case ")
	  (write-js-expression thing)))
    (write-js-stream ":")))

(def-js-syntax js-syntax:default ()
  (asserting-js-statement
    (write-js-statement '(js-syntax:case js-syntax:default))))

(def-js-syntax js-syntax:cases (things &rest body)
  (asserting-js-statement
    (loop for thing in things
       do (write-js-statement `(js-syntax:case ,thing)))
    (write-js-program body)
    (write-js-statement `(js-syntax:break *js-switch-name*))))

(def-js-syntax js-syntax:throw (thing)
  (in-js-statement
    (write-js-stream "throw ")
    (write-js-expression thing)))

(defun is-block (thing)
  (and (consp thing) (eq (car thing) 'js-syntax:block)))

(def-js-syntax js-syntax:try (try &rest catch-finally)
  (asserting-js-statement
    (write-js-stream "try")
    (assert (is-block try) () "the try form must be a block")
    (write-js-statement try)
    (when catch-finally
      (let ((f (first catch-finally)))
	(if (is-block f)
	    (progn
	      (write-js-stream "finally")
	      (write-js-statement f))
	    (progn
	      (if (not (null f))
		  (if (and (consp f)
			   (eq (car f) 'js-syntax:catch))
		      (write-js-statement f)
		      (error "The catch form must be (catch ...), the finally form must be a block. Got ~a" f))
		  (when (and (cdr catch-finally)
			     (is-block (second catch-finally)))
		    (write-js-stream "finally")
		    (write-js-statement f)))))))))

(def-js-syntax js-syntax:catch (var &rest block)
  (write-js-stream "catch(")
  (write-js-var var)
  (write-js-stream ")")
  (write-js-statement `(js-syntax:block ,@block)))

(def-js-syntax js-syntax:function (args &rest body)
  (if (atom args)
      (write-js-thing `(js-syntax:named-function ,args ,@body))
      (write-js-thing `(js-syntax:named-function "" ,args ,@body))))

(def-js-syntax js-syntax:named-function (name args &rest body)
  (typecase name
    (symbol
     (write-js-thing `(js-syntax:named-function ,(get-js-var-string name) ,args ,@body)))
    (string
     (let ((b (or (not *js-statement*) (zerop (length name)))))
       (when b (write-js-stream "("))
       (if (plusp (length name))
	   (progn
	     (write-js-stream "function ")
	     (write-js-stream name)
	     (setf *js-statement-written* t)) ;named functions are statements, anonymous are expressions
	   (write-js-stream "function"))
       (write-js-stream "(")
       (loop for (arg . rest) on args
	  do (write-js-thing arg)
	    (when rest (write-js-stream ",")))
       (write-js-stream ")")
       (write-js-statement `(js-syntax:block ,@body))
       (when b (write-js-stream ")"))))))


(def-js-syntax js-syntax:true () (write-js-stream "true") ;;!0 instead?
	       )
(def-js-syntax js-syntax:false () (write-js-stream "false") ;;!1 instead?
	       )
(def-js-syntax js-syntax:null () (write-js-stream "null"))
(def-js-syntax js-syntax:undefined () (write-js-stream "undefined"))  ;; void(0) instead?
(def-js-syntax js-syntax:this () (write-js-stream "this"))

(def-js-syntax js-syntax:number (n)
  (write-js-stream (let ((*read-default-float-format* 'double-float))
		     (write-to-string
		      (etypecase n
			(integer n)
			(double-float n)
			(real (coerce n 'double-float)))
		      :readably t))))

(defmacro string-escaper ((strvar &optional (escape-char #\\) surround) &body escape-list)
  (let ((escape-count (gensym "COUNT"))
	(char (gensym "CHAR"))
	(new (gensym "NEW"))
	(j (gensym "J")))
    `(let ((,escape-count 0))
       (loop for ,char across ,strvar
	  if (member ,char ',(mapcar (lambda (x) (if (consp x) (car x) x)) escape-list))
	  do (incf ,escape-count))
       (if (zerop ,escape-count)
	   ,(if surround
		`(let ((,new (make-string (+ (length ,strvar) 2))))
		   (setf (schar ,new 0) ,surround)
		   (setf (subseq ,new 1) ,strvar)
		   (setf (schar ,new (1- (length ,new))) ,surround)
		   ,new)
		strvar)
	   (let ((,new (make-string (+ (length ,strvar) ,escape-count
				       ,@(when surround (list 2))))))
	     ,(if surround `(setf (schar ,new 0) ,surround))
	     (loop for ,char across ,strvar
		with ,j = ,(if surround 1 0)
		do (case ,char
		     ,@(loop for x in escape-list
			    if (characterp x) collect `(,x (setf (schar ,new ,j) ,escape-char)
							   (setf (schar ,new (1+ ,j)) ,x)
							   (incf ,j 2))
			  else collect `(,(car x) (setf (schar ,new ,j) ,escape-char)
					  (setf (schar ,new (1+ ,j))
						,(if (consp (cdr x)) (cadr x) (cdr x)))
					  (incf ,j 2)))
		     (t (setf (schar ,new ,j) ,char) (incf ,j)))
		  ,@(if surround `(finally (setf (schar ,new ,j) ,surround))))
	     ,new)))))

(defvar *js-string-quote* #\')

(def-js-syntax js-syntax:string (str)
  (let ((estr
	 (ecase *js-string-quote*
	   (#\' (string-escaper (str #\\ #\')
		  #\' #\\ (#\Newline #\n) (#\Return #\r)))
	   (#\" (string-escaper (str #\\ #\")
		  #\" #\\ (#\Newline #\n) (#\Return #\r))))))
    (write-js-stream estr)))


(def-js-syntax js-syntax:vector (&rest elems)
  (write-js-stream "[")
  (if elems
      (write-js-expression `(js-syntax:\, ,@elems) :level 0 :can-have-brackets nil))
  (write-js-stream "]"))

(def-js-syntax js-syntax:object (&rest key-value-pairs)
  (write-js-stream "{")
  (loop for ((key value) . rest) on key-value-pairs
     do (assert (or (stringp key) (and (consp key) (eq (car key) 'js:string)))
		()
		"Object keys must be strings") ;todo: support {[thing]:val} syntax
       (write-js-syntax 'js:string (if (consp key) (cdr key) (list key)))
       (write-js-stream ":")
       (write-js-expression value :level 1)
       (if rest (write-js-stream ",")))
  (write-js-stream "}"))


(defmacro def-js-binop (name str &key zero-expr one-arg one-arg-expr unary-space
				   (chain-extra-args t)
				   (level 0) (sublevel (1+ level)) (unary-level 14) (unary-sublevel 14))
  (let ((args (gensym "ARGS"))
	(first-arg (or (and (not (keywordp one-arg)) one-arg) (gensym "FIRST")))
	(second-arg (gensym "SECOND"))
	(one-arg-unary (eq one-arg :unary)))
    (multiple-value-bind (lambda-list special-cases final-bind)
	(cond
	  ((and zero-expr one-arg-expr)
	   (values
	    `(&rest ,args)
	    `(((null ,args) (write-js-expression ,zero-expr))
	      ((null (cdr ,args)) (write-js-expression
				   (let ((,first-arg (car ,args)))
				     (declare (ignorable ,first-arg))
				     ,one-arg-expr))))
	    args))
	  (zero-expr ;zero: yes, one: no
	   (values
	    `(&rest ,args)
	    `(((null ,args) (write-js-expression ,zero-expr))
	      ,@(unless one-arg-unary
		  `(((null (cdr ,args))
		     (error ,(format nil "js operator ~w expects zero, two or more arguments" name))))))
	    args))
	  (one-arg-expr ;zero: no, one: yes
	   (values
	    `(,first-arg &rest ,args)
	    `(((null ,args) (write-js-expression
			      (let ((,first-arg (car ,args)))
				(declare (ignorable ,first-arg))
				,one-arg-expr))))
	    `(cons ,first-arg ,args)))
	  (one-arg-unary
	   (values
	    `(,first-arg &rest ,args)
	    `()
	    `(cons ,first-arg ,args)))
	  (t ;two-or-more
	   (values
	    `(,first-arg ,second-arg &rest ,args)
	    `()
	    `(list* ,first-arg ,second-arg ,args))))
      `(def-js-syntax ,name ,lambda-list
	 (at-expression-level (,level ,sublevel)
	   (cond ,@special-cases
		 (t ,(if chain-extra-args
			 `(let ((,args ,final-bind))
			    (if (null (cdr ,args)) ;we use this to check length=1 as the expression may repeat
				(at-expression-level (,unary-level ,unary-sublevel)
				  (write-js-stream ,str)
				  ,(if unary-space '(write-js-stream " "))
				  (write-js-expression (first ,args)))
				(loop for (,first-arg . ,second-arg) on ,args
				   do (write-js-expression ,first-arg)
				     (when ,second-arg (write-js-stream ,str)))))
			 `(let* ((,args ,final-bind)
				 (,first-arg (first ,args))
				 (,second-arg (second ,args)))
			    ,@(if (eq one-arg :unary)
				  `((assert (<= 1 (length ,args) 2) () "Operator ~a must have 1 or 2 args" ',name)
				    (if (cdr ,args)
					(progn
					  (write-js-stream ,str)
					  ,(if unary-space '(write-js-stream " "))
					  (write-js-expression ,first-arg))
					(progn
					  (write-js-expression ,first-arg)
					  (write-js-stream ,str)
					  (write-js-expression ,second-arg))))
				  `((assert (= (length ,args) 2) () "Operator ~a must have 2 args" ',name)
				    (write-js-expression ,first-arg)
				    (write-js-stream ,str)
				    (write-js-expression ,second-arg))))))))))))

(defmacro def-js-unaryop (op str &key level (sublevel level) (sublevel-brackets t) (fix :prefix))
  (let* ((x (gensym "X"))
	 (e `(write-js-expression ,x :level ,sublevel :can-have-brackets ,sublevel-brackets)))
    `(def-js-syntax ,op (,x)
       (at-expression-level (,level)
	 ,@(ecase fix
	     (:prefix
	      (list `(write-js-stream ,str) e))
	     (:postfix
	      (list e `(write-js-stream ,str))))))))

(def-js-syntax js-syntax:\. (first second &rest others)
  (at-expression-level (18 18)
    (write-js-expression first)
    (flet ((dot-or-brackets (thing)
	     (if (is-valid-js-identifier thing)
		 (progn (write-js-stream ".") (write-js-stream thing))
		 (progn
		   (write-js-stream "[")
		   (if (stringp thing)
		       (write-js-expression `(js-syntax:string ,thing))
		       (write-js-expression thing))
		   (write-js-stream "]")))))
      (loop for x in (cons second others)
	   do (dot-or-brackets x)))))

(def-js-syntax js-syntax:new (thing &rest arguments)
  (if arguments
      (progn
	(at-expression-level (18 18)
	  (write-js-stream "new ")
	  (write-js-expression thing)
	  (write-js-stream "(")
	  (write-js-expression `(js-syntax:\, ,@arguments) :level 0 :can-have-brackets nil)
	  (write-js-stream ")")))
      (if (<= *js-expression-level* 16)
	  (progn
	    (write-js-stream "new ")
	    (write-js-expression thing :level 18))
	  (at-expression-level (18 18)
	    (write-js-stream "new ")
	    (write-js-expression thing)
	    (write-js-stream "()")))))

(def-js-syntax js-syntax:call (function &rest arguments)
  (at-expression-level (17 17)
    (write-js-expression function)
    (write-js-stream "(")
    (if arguments
	(write-js-expression `(js-syntax:\, ,@arguments) :level 0 :can-have-brackets nil))
    (write-js-stream ")")))

(def-js-unaryop js-syntax:post++ "++" :level 15 :sublevel 16 :sublevel-brackets nil :fix :postfix)
(def-js-unaryop js-syntax:post-- "--" :level 15 :sublevel 16 :sublevel-brackets nil :fix :postfix)

(def-js-unaryop js-syntax:! "!" :level 14)
(def-js-unaryop js-syntax:~ "~" :level 14)
(def-js-unaryop js-syntax:pre++ "++" :level 14 :sublevel 16 :sublevel-brackets nil)
(def-js-unaryop js-syntax:pre-- "--" :level 14 :sublevel 16 :sublevel-brackets nil)
(def-js-unaryop js-syntax:delete "delete " :level 14 :sublevel 16) ;delete removes a key from an object
(def-js-unaryop js-syntax:typeof "typeof " :level 14)
(def-js-unaryop js-syntax:void "void " :level 14) ;void x evaluates x and returns undefined



(def-js-binop js-syntax:* "*"
  :zero-expr '(js-syntax:number 1)
  :one-arg x :one-arg-expr x
  :level 13)
(def-js-binop js-syntax:/ "/"
  :one-arg x :one-arg-expr `(js-syntax:/ (js-syntax:number 1) ,x)
  :level 13)
(def-js-binop js-syntax:% "%" :chain-extra-args nil :level 13)

(def-js-binop js-syntax:- "-"
  :level 12
  :one-arg :unary
  :unary-space t) ;we have a space because (- (number -1)) should be - -1, not --1 (an error)

(def-js-binop js-syntax:+ "+"
  :level 11
  :zero-expr '(js-syntax:number 0)
  :one-arg :unary :level 11)

(def-js-binop js-syntax:<< "<<" :chain-extra-args nil :level 10)
(def-js-binop js-syntax:>> ">>" :chain-extra-args nil :level 10)
(def-js-binop js-syntax:>> ">>>" :chain-extra-args nil :level 10)

(def-js-binop js-syntax:< "<" :chain-extra-args nil :level 9)
(def-js-binop js-syntax:> ">" :chain-extra-args nil :level 9)
(def-js-binop js-syntax:<= "<=" :chain-extra-args nil :level 9)
(def-js-binop js-syntax:>= ">=" :chain-extra-args nil :level 9)
(def-js-binop js-syntax:instanceof " instanceof " :chain-extra-args nil :level 9)
(def-js-binop js-syntax:in " in " :chain-extra-args nil :level 9)

(def-js-binop js-syntax:== "==" :chain-extra-args nil :level 8)
(def-js-binop js-syntax:!= "!=" :chain-extra-args nil :level 8)
(def-js-binop js-syntax:=== "===" :chain-extra-args nil :level 8)
(def-js-binop js-syntax:!== "!==" :chain-extra-args nil :level 8)

(def-js-binop js-syntax:& "&" :level 7)

(def-js-binop js-syntax:^ "^" :level 6)

(def-js-binop js-syntax:\| "|" :level 5)

(def-js-binop js-syntax:&& "&&" :level 4
	      :zero-expr '(js-syntax:true)
	      :one-arg x :one-arg-expr x)

(def-js-binop js-syntax:\|\| "||" :level 3
	      :zero-expr '(js-syntax:false)
	      :one-arg x :one-arg-expr x)

(def-js-syntax js-syntax:? (test then else)
  (at-expression-level (2)
    (write-js-expression test :level 3)
    (write-js-stream "?")
    (write-js-expression then :level 1)
    (write-js-stream ":")
    (write-js-expression else :level 1)))

(defmacro def-js-assignemt (op str)
  (let ((left (gensym "LEFT"))
	(right (gensym "RIGHT"))
	(others (gensym "OTHERS")))
    `(def-js-syntax ,op (,left ,right &rest ,others)
       (if ,others
	   (write-js-expression `(,',op ,,left (,',op ,,right ,@,others)))
	   (at-expression-level (1)
	     (write-js-expression ,left :level 16 :can-have-brackets nil)
	     ;;we disable brackets => can't have lower-precedence operators at top-level
	     (write-js-stream ,str)
	     (write-js-expression ,right :level 1))))))

(def-js-assignemt js-syntax:= "=")
(def-js-assignemt js-syntax:*= "*=")
(def-js-assignemt js-syntax:/= "/=")
(def-js-assignemt js-syntax:%= "%=")
(def-js-assignemt js-syntax:+= "+=")
(def-js-assignemt js-syntax:-= "-=")
(def-js-assignemt js-syntax:<<= "<<=")
(def-js-assignemt js-syntax:>>= ">>=")
(def-js-assignemt js-syntax:>>>= ">>>=")
(def-js-assignemt js-syntax:&= "&=")
(def-js-assignemt js-syntax:^= "^=")
(def-js-assignemt js-syntax:\|= "|=")

(def-js-binop js-syntax:\, ","
  :zero-expr '(js-syntax:void (js-syntax:number 0))
  :one-arg x :one-arg-expr x
  :level 0)
