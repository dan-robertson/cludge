;;; Here we have an implementation of the relooper
;;; algorithm from Emscripten. This is based on the
;;; C++ implementation in Emscripten (and Shumway).

(cl:in-package #:js-relooper)

;;; some special variables
(defvar *shape-id-counter* 0)
(defvar *block-id-counter* 1) ;0 is reserved for clearings
(defvar *relooper-label-var* (gensym "RELOOPER-LABEL"))

;;; DEBUG
(declaim (optimize (debug 3)))

;;; FOR BRANCH

(defun js-unblock (js)
  (cond
    ((null js) nil)
    ((atom js) (list js))
    ((and (consp js) (eq (car js) 'js:empty)) nil)
    ((and (consp js) (eq (car js) 'js:block)) (cdr js))
    (t (list js))))

(defun js-maybe-block (x)
  (cond
    ((null x) '(js:empty))
    ((atom x) x)
    ((and (consp x) (atom (car x))) x)
    ((= 1 (length x)) (first x))
    (t (cons 'js:block x))))

(defun label-for-id (id)
  (format nil "L~d" id))

(defun render-branch (branch target set-label)
  (append (js-unblock (code branch))
	  (if set-label `((js:= ,*relooper-label-var* ,(id target))))
	  (and (ancestor branch)
	       (member (type branch) '(:break :continue))
	       (let ((op (ecase (type branch) (:break 'js:break) (:continue 'js:continue))))
		 (if (labelled branch)
		     `((,op ,(label-for-id (id (ancestor branch)))))
		     `((,op)))))))

(defun make-branch (condition code)
  (make-instance 'branch :condition condition :code code))
;;; TODO: make-branch

;;; FOR BLOCK

(defun add-branch (from to &optional condition code)
  (assert (not (assoc to (branches-out from))) () "Cannot re-add branch ~a to block ~a" to from)
  (setf (branches-out from) (acons to (make-branch condition code) (branches-out from))))

(defun process-non-switch-body (body)
  (if (null body)
      '(js:empty)
      (let ((first (first body)))
	(assert (and (consp first) (eq (car first) 'condition)) () "Body must be in case-like format")
	(let ((condition (cdr first)))
	  (if condition
	      (loop for x on (rest body)
		 if (and (consp x) (consp (car x)) (eq (caar x) 'condition))
		 return `(js:if ,condition
				,(js-maybe-block then)
				,(if (cdr x)
				     (process-non-switch-body x)))
		 else collect (car x) into then
		 finally (return `(js:if ,condition ,(js-maybe-block then))))
	      (loop for x on (rest body)
		 if (and (consp x) (consp (car x)) (eq (caar x) 'condition))
		 do (error "case found after final (unconditional) case")
		 finally (return (js-maybe-block (rest body)))))))))

(defun maybe-switch (use-switch switch-on body)
  ;; either a switch or an if-elseif chain
  (if use-switch
      `(js:switch (,switch-on) ,@body)
      ;; not a switch so body = ((condition . C1) CODE1... (condition . C2) CODE2... ...)
      (process-non-switch-body body)))

(defun maybe-render-loop (should-loop fused body)
  (if should-loop
      (list (render-loop fused body))
      (list body)))

(defun render-block (block in-loop)
  (declare (optimize (debug 3)))
  (append (if (and in-loop (checked-multiple-entry-p block))
	      `((js:= ,*relooper-label-var* 0)))
	  (js-unblock (code block))
	  (when (processed-branches-out block)
	    (let* ((set-label t)
		   (branch-var (branch-var block))
		   (force-set-label (emulated-p (parent block)))
		   (fused (next (parent block)))
		   (fused-p (multiple-p fused))
		   (processed-branches-out-default-last
		    (let ((dt nil) (dtt nil))
		      (prog1
			  (loop for ((blk . branch) . rest) on (processed-branches-out block)
			     if (condition branch)
			     collect (cons blk branch)
			     else
			     do (assert (null dt) () "There must only be one default target")
			       (setf dt blk dtt branch)
			     end
			     if (null rest) collect (cons dt dtt))
			(assert dt () "There must be some default target"))))
		   (use-switch (not (null branch-var))))
	      (progn
	       (when fused-p
		 (setf (next (parent block))
		       (next (next (parent block)))
		       (use-switch fused) nil)
		 (if (and set-label
			  (= (length (inner-map fused))
			     (length (processed-branches-out block))))
		     (setf set-label nil)))
	       (maybe-render-loop
		fused-p fused
		(maybe-switch
		 use-switch branch-var
		 (loop for ((target . details) . rest) on processed-branches-out-default-last
		    with remaining-conditions
		    for last = (null rest)
		    for set-current-label = (or (and set-label (checked-multiple-entry-p target))
		    			       force-set-label)
		    for has-fused-content = (and fused-p (assoc (id target) (inner-map fused)))
		    for has-content = (or set-current-label
		    			 (not (eq :direct (type details)))
		    			 has-fused-content
		    			 (code details))
		    if (not last)
		      if use-switch
		        collect (condition details)
		      else
		        if has-content
		          collect (cons 'condition (condition details))
		        else
		          do (setf remaining-conditions
				   (cons `(js:! ,(if branch-var `(js:=== ,branch-var
		    							 ,(condition details))
		    				     (condition details)))
					 remaining-conditions))
		        end
		      end
		    else
		      ;;this is the defailt branch
		      if use-switch
		        collect '(js:default)
		      else
		        if has-content
		          collect (cons 'condition (cons 'js:&& (reverse remaining-conditions)))
		        end
		      end
		    end
		    append (render-branch details target set-current-label)
		    if has-fused-content
		      append (render-shape (cdr has-fused-content) in-loop)
		    else
		      when (eq :nested (type details))
		        do (assert (next (parent block)))
		        and append (render-shape (next (parent block)) in-loop)
		        and do (setf (next (parent block)) nil)
		      end
		    end
		    when (and use-switch (not last)) collect '(js:break)))))))))


(defun make-block (code branch-var)
  (make-instance 'block :code code :branch-var branch-var))
;;; TODO: make-block

;;; FOR shape:

(defgeneric render-shape (shape in-loop))
;;; TODO render function (in-loop)


(defmethod render-shape ((shape simple-shape) in-loop)
  (append (render-block (inner shape) in-loop)
	  (if (next shape) (render-shape (next shape) in-loop))))


(defun render-loop (multiple-shape body)
  (if (breaks multiple-shape)
      (if (use-switch multiple-shape)
	  (if (and (consp body) (eq (car body) 'js:switch)
		   ;body = (js:switch (on [label]) . body)
		   (null (cadadr body)))
	      `(js:switch (,(caadr body) ,@(if (labelled multiple-shape)
					       (label-for-id (id multiple-shape))))
			  ,@(cddr body))
	      body)
	  `(js:do-while ,body (js:number 0)
			,@(if (labelled multiple-shape)
			      (label-for-id (id multiple-shape)))))
      body))

(defun render-multiple-shape-if-else-chain (list in-loop)
  (let ((first (first list))
	(rest (rest list)))
    (let ((id (car first))
	  (shape (cdr first)))
      `(js:if (js:=== ,*relooper-label-var* ,id)
	      ,(js-maybe-block (render-shape shape in-loop))
	      ,@(when rest (render-multiple-shape-if-else-chain rest in-loop))))))

(defmethod render-shape ((shape multiple-shape) in-loop)
  (cons
   (render-loop
    shape
    (if (use-switch shape)
	`(js:switch
	  (,*relooper-label-var*)
	  ,@(loop for (id . ishape) in (inner-map shape)
	       collect `(js:cases ((js:number ,id))
				  ,@(render-shape ishape in-loop))))
	(render-multiple-shape-if-else-chain (inner-map shape) in-loop)))
   (if (next shape) (render-shape (next shape) in-loop))))

(defmethod render-shape ((shape loop-shape) in-loop)
  (cons
   `(js:while (js:number 1)
      (js:block ,@(render-shape (inner shape) t))
      ,@(if (labelled shape) (label-for-id (id shape))))
   (if (next shape) (render-shape (next shape) in-loop))))

(defmethod render-shape ((shape emulated-shape) in-loop)
  (list*
   `(js:= ,*relooper-label-var* (js:number ,(id (entry shape))))
   `(js:while (js:number 1)
      (js:switch
       (,*relooper-label-var*)
       (loop for block in (blocks shape)
	  collect `(js:cases ((js:number ,(id block)))
			     ,@(render-block block in-loop))))
      ,@(if (labelled shape) (label-for-id (id shape))))
   (if (next shape) (render-shape (next shape) in-loop))))


;;; we need double-ended-queues. We implement simply using an adjustable vector
;;; like a ring-buffer. This is based on the assumption that we use them as follows:
;;; First we make the queue and add a bunch of stuff to the back of it.
;;; Then we take elements off the front and sometimes add things to the back; the queue
;;; shrinking over this time until we stop using it
(defstruct (deque (:conc-name dq-))
  (storage (make-array '(64) :element-type t :initial-element nil
		       :adjustable t)
	   :type (array t (*)))
  (start 0 :type fixnum) ;the index of the first element
  (length 0 :type fixnum)
  )

(defun dq-wraps-p (start filled-length storage-length)
  (declare (cl:type fixnum start filled-length storage-length)
	   (inline dq-wraps-p))
  (> (+ start filled-length) storage-length))

(defun dq-extend (dq &optional (ensure-space-for 1))
  (declare (cl:type deque dq)
	   (cl:type fixnum ensure-space-for))
  (with-accessors ((storage dq-storage) (start dq-start) (length dq-length)) dq
    (let* ((len (length storage))
	   (gap (- len length)))
      (if (<= ensure-space-for gap)
	  nil
	  (let ((newlen (loop for nl = (* len 2) then (* nl 2)
			   and added = len then (+ added nl)
			   when (<= ensure-space-for added) return nl)))
	    (adjust-array storage (list newlen) :initial-element nil)
	    ;; we now may need to move part of the contents if the
	    ;; elements are stored across the boundry between start and end
	    (when (dq-wraps-p start length len)
	      ;; before:    (--------END     START--------)
	      ;; currently: (--------END     START--------                        )
	      ;; option a:  (                START----------------END             )
	      ;; option b:  (--------END                             START--------)
	      (let ((startmove (- len start)))
		(if (< length (+ startmove startmove))
		    ;; we take option a
		    (loop for read fixnum from 0 below (- length startmove)
		       for write fixnum from len
		       do (setf (aref storage write) (aref storage read)
				(aref storage read) nil))
		    ;; we take option b
		    (loop for read fixnum downfrom (1- len) to start
		       for write fixnum downfrom (1- newlen)
		       do (setf (aref storage write) (aref storage read)
				(aref storage read) nil)
		       finally (setf start write)))
		nil)))))))

(defun dq-push-front (dq item)
  (declare (cl:type deque dq))
  (dq-extend dq)
  (if (= 0 (dq-start dq))
      (setf (aref (dq-storage dq)
		  (setf (dq-start dq) (1- (length (dq-storage dq)))))
	    item)
      (setf (aref (dq-storage dq) (decf (dq-start dq))) item))
  (incf (dq-length dq))
  nil)
(defun dq-pop-front (dq)
  (declare (cl:type deque dq))
  (assert (plusp (dq-length dq)) () "Cannot pop from empty deque")
  (let ((result nil))
    (rotatef (aref (dq-storage dq) (dq-start dq)) result)
    (if (= (length (dq-storage dq)) (1+ (dq-start dq))) ;is start at the end?
	(setf (dq-start dq) 0)
	(incf (dq-start dq)))
    (decf (dq-length dq))
    result))
(defun dq-push-back (dq item)
  (declare (cl:type deque dq))
  (let ((place (mod (+ (dq-start dq) (dq-length dq))
		    (length (dq-storage dq)))))
    (setf (aref (dq-storage dq) place) item)
    (incf (dq-length dq)))
  nil)
(defun dq-pop-back (dq)
  (declare (cl:type deque dq))
  (assert (plusp (dq-length dq)) () "Cannot pop from empty deque")
  (let ((result nil))
    (let ((place (mod (+ (dq-start dq) (1- (dq-length dq)))
		    (length (dq-storage dq)))))
      (rotatef (aref (dq-storage dq) place) result)
      (decf (dq-length dq)))
    result))
(declaim (inline dqref))
(defun dqref (dq n)
  (declare (cl:type deque dq) (cl:type fixnum n))
  (assert (< n (dq-length dq)))
  (aref (dq-storage dq) (mod (+ (dq-start dq) n) (length (dq-storage dq)))))
(declaim (inline (setf dqref)))
(defun (setf dqref) (newvalue dq n)
  (declare (cl:type deque dq) (cl:type fixnum n))
  (assert (< n (dq-length dq)))
  (setf (aref (dq-storage dq) (mod (+ (dq-start dq) n) (length (dq-storage dq))))
	newvalue))


(defstruct relooper
  (blocks (make-deque))				;double-ended queue of blocks
  (shapes (make-deque))				;double-ended queue of shapes
  (root nil :type (or null shape))
  (emulate nil :type boolean)

  )

;;; these are the variables
;; (defvar *shape-id-counter* 0)
;; (defvar *block-id-counter* 1) ;0 is reserved for clearings
;; (defvar *relooper-label-var* (gensym "RELOOPER-LABEL"))
(defmacro postincf (thing &optional (delta 1))
  `(- (incf ,thing ,delta) ,delta))

(defun relooper-add-block (rl block &optional (id (postincf *block-id-counter*)))
  (setf (id block) id)
  (dq-push-back (relooper-blocks rl) block))

(defclass relooper-recursor ()
  ((parent :initform nil
	   :initarg :parent
	   :accessor parent
	   :type relooper)))

(defclass pre-optimiser (relooper-recursor)
  ((live :initform nil ;set of blocks --implemented as list
	 :initarg :live
	 :accessor live)))

(defun find-live (pre-optimiser root)
  (let ((edge (make-deque)))
    (dq-push-back edge root)
    (cl:block nil
      (loop
	 (if (plusp (dq-length edge))
	   (let ((current (dq-pop-front edge)))
	     (unless (member current (live pre-optimiser))
	       (push current (live pre-optimiser))
	       (loop for (block . nil) in (branches-out current)
		  do (dq-push-back edge block))))
	   (return))))
    (live pre-optimiser)))

(defun estimate-js-size (js)
  (labels ((recur (js &aux (count-so-far 0))
	     (if (atom js)
		 (if (stringp js) (length js) 3)
		 (let ((cdr (if (eq (car js) 'js:switch)
				(progn (incf count-so-far (1- (length (cadr js))))
				       (cons (caadr js) (cddr js)))
				(cdr js))))
		   (incf count-so-far
			 (if (eq (car js) 'js:call)
			     2
			     (if (symbolp (car js)) (length (symbol-name (car js)))
				 5)))
		   (loop for x in cdr do (incf count-so-far (recur x)))
		   count-so-far))))
    (if js
	(if (and (consp js) (consp (car js)))
	    (loop for x in js sum (recur x))
	    (recur js))
	0)))

(defun split-dead-ends (pre-optimiser)
  (let ((total-code-size (loop for block in (live pre-optimiser)
			      sum (estimate-js-size (code block))))
	splits removed)			;sets (lists) of blocks
    (loop for original in (live pre-optimiser)
       when (and (zerop (length (branches-out original)))
		 (> 1 (length (branches-in original)))
		 (not (member original (branches-out original)))
		 (<= (* (1- (length (branches-in original)))
			(estimate-js-size (code original)))
		     (/ total-code-size 5)))
       ;; split the node
       do (loop for prior in (branches-in original)
	     for split = (make-block (code original) (branch-var original))
	     for (nil . details) = (assoc original (branches-out prior))
	     do (relooper-add-block (parent pre-optimiser) split (id original))
	       (push prior (branches-in split))
	       (push (cons split (make-branch (condition details) (code details)))
		     (branches-out prior))
	       (setf (branches-out prior)
		     (remove original (branches-out prior)
			     :key #'car))
	       (loop for (post . details) in (branches-out original)
		  do (push (cons post (make-branch (condition details) (code details)))
			   (branches-out split))
		    (push split (branches-in post)))
	       (push split splits)
	       (pushnew original removed))
	 (loop for (post . nil) in (branches-out original)
	    do (setf (branches-in post)
		     (remove original (branches-in post)))))
    (setf (live pre-optimiser)
	  (union (set-difference (live pre-optimiser) removed)
		 splits))))


(defclass analyser (relooper-recursor) ())

(defun notice (analyser new)
  (declare (cl:type analyser analyser)
	   (cl:type shape new))
  (setf (id new) (postincf *shape-id-counter*))
  (dq-push-back (relooper-shapes (parent analyser)) new))


;;; We define this early as it is a macro referred to later.
(defmacro process& (analyser blocks initial-entries previous &environment env)
  (multiple-value-bind (vars vals store-vars writer-form reader-form)
      (get-setf-expansion blocks env)
    (multiple-value-bind (vars2 vals2 store-vars2 writer-form2 reader-form2)
	(get-setf-expansion initial-entries)
      (let ((av (gensym))
	    (new (car store-vars))
	    (new2 (car store-vars2))
	    (r (gensym)))
	(when (cdr store-vars) (error "can't expand this"))
	(when (cdr store-vars2) (error "can't expand this"))
	`(let* ((,av ,analyser)
		,@(mapcar #'list vars vals)
		(,new ,reader-form)
		,@(mapcar #'list vars2 vals2)
		(,new2 ,reader-form2))
	   (multiple-value-bind (,r ,new ,new2)
	       (process ,av ,new ,new2 ,previous)
	     ,writer-form
	     ,writer-form2
	     ,r))))))

(defun get-blocks-out (analyser source entries &optional (limit-to nil limit-to-p))
  (declare (ignore analyser))
  ;; source is a block; entries a set of blocks; limit-to a set of blocks
  ;; returns updated entries with branches out added
  (loop for (block . nil) in (branches-out source)
     if (or (not limit-to-p) (null limit-to) (member block limit-to))
     do (pushnew block entries))
  entries)

(defmacro get-blocks-out& (analyser source entries &optional (limit-to nil ltp) &environment env)
  (multiple-value-bind (vars vals store-vars writer-form reader-form)
      (get-setf-expansion entries env)
    (let ((new (first store-vars))
	  (av (gensym)) (sv (gensym)))
      (when (cdr store-vars) (error "can't expand this"))
      `(let* ((,av ,analyser)
	      (,sv ,source)
	      ,@(mapcar #'list vars vals)
	      (,new ,reader-form))
	 (setf ,new (get-blocks-out ,av ,sv ,new ,@(if ltp (list limit-to))))
	 ,writer-form
	 nil))))

(defun solipsize (analyser target type ancestor from)
  (declare (cl:type analyser analyser)
	   (cl:type block target)
	   (cl:type (member :direct :break :continue :nested) type)
	   (cl:type list from) ;set of blocks
	   (ignore analyser)
	   )
  (setf (branches-in target)
	(loop for prior in (branches-in target)
	   if (member prior from)
	     ;; implicitly remove prior -- i.e. don't collect it
	     do (let ((prior-out (cdr (assoc target (branches-out prior)))))
		  (setf (ancestor prior-out) ancestor
			(type prior-out) type)
		  (when (multiple-p ancestor) (incf (breaks ancestor)))
		  (pushnew prior (processed-branches-in target))
		  (setf (branches-out prior)
			(remove target (branches-out prior) :key #'car))
		  (pushnew (cons target prior-out) (processed-branches-out prior)))
	   else collect prior))
  nil)

(defun make-simple (analyser blocks inner next-entries)
  (declare (cl:type analyser analyser)
	   (cl:type list blocks next-entries) ;sets of blocks. we modify by returning modified lists
	   (cl:type block inner))
  (let ((simple (make-instance 'simple-shape
			       :inner inner)))
    (notice analyser simple)
    (setf (parent inner) simple)
    (if (> (length blocks) 1)
	(progn
	  (setf blocks (remove inner blocks))
	  (get-blocks-out& analyser inner next-entries blocks)
	  (loop for x in next-entries
	     with inn = (list inner)
	     do (solipsize analyser x :direct simple inn))
	  (values simple blocks next-entries))
	(values simple blocks next-entries))))

(defmacro make-simple& (analyser blocks inner next-entries &environment env)
  (multiple-value-bind (vars vals store-vars writer-form reader-form)
      (get-setf-expansion blocks env)
    (multiple-value-bind (vars2 vals2 store-vars2 writer-form2 reader-form2)
	(get-setf-expansion next-entries)
      (let ((av (gensym))
	    (iv (gensym))
	    (new (car store-vars))
	    (new2 (car store-vars2))
	    (r (gensym)))
	(when (cdr store-vars) (error "can't expand this"))
	(when (cdr store-vars2) (error "can't expand this"))
	`(let* ((,av ,analyser)
		,@(mapcar #'list vars vals)
		(,new ,reader-form)
		(,iv ,inner)
		,@(mapcar #'list vars2 vals2)
		(,new2 ,reader-form2))
	   (multiple-value-bind (,r ,new ,new2)
	       (make-simple ,av ,new ,iv ,new2)
	     ,writer-form
	     ,writer-form2
	     ,r))))))

(defun make-emulated (analyser blocks entry next-entries)
  (declare (cl:type analyser analyser)
	   (cl:type list blocks next-entries)
	   (cl:type block entry)
	   (ignore next-entries))
  (let ((emulated (make-instance 'emulated-shape
				 :entry entry
				 :blocks blocks)))
    (notice analyser emulated)
    (loop for current in blocks
       do (setf (parent current) emulated)
	 (solipsize analyser current :continue emulated blocks))
    (values emulated nil)))

(defmacro make-emulated& (analyser blocks entry next-entries &environment env)
  (multiple-value-bind (vars vals store-vars writer-form reader-form)
      (get-setf-expansion blocks env)
    (let ((av (gensym))
	  (r (gensym))
	  (new (car store-vars)))
      (when (cdr store-vars) (error "Can't expand this"))
      `(let* ((,av ,analyser)
	      ,@(mapcar #'list vars vals)
	      (,new ,reader-form))
	 (multiple-value-bind (,r ,new)
	     (make-emulated ,av ,new ,entry ,next-entries)
	   ,writer-form
	   ,r)))))

(defun make-loop (analyser blocks entries next-entries)
  (declare (cl:type analyser analyser)
	   (cl:type list blocks entries next-entries)) ;all block sets
  (let (inner-blocks (queue entries))
    (loop
       while queue
       do (let ((current (pop queue)))
	    (unless (member current inner-blocks)
	      (push current inner-blocks)
	      (setf blocks (remove current blocks))
	      (setf queue (append (branches-in current) queue)))))
    (assert inner-blocks () "Loop must have some inner blocks")
    (loop for current in inner-blocks
       do (loop for (possible . nil) in (branches-out current)
	     unless (member possible inner-blocks)
	     do (pushnew possible next-entries)))
    (let ((loop (make-instance 'loop-shape)))
      (notice analyser loop)
      (loop for item in entries
	 do (solipsize analyser item :continue loop inner-blocks))
      (loop for item in next-entries
	 do (solipsize analyser item :break loop inner-blocks))
      (let ((inner (process& analyser inner-blocks entries nil)))
	(setf (inner loop) inner)
	(values loop blocks entries next-entries)))))
(defmacro make-loop& (analyser blocks entries next-entries &environment env)
  (multiple-value-bind (vars vals store-vars writer-form reader-form)
      (get-setf-expansion blocks env)
    (multiple-value-bind (vars2 vals2 store-vars2 writer-form2 reader-form2)
	(get-setf-expansion entries)
      (multiple-value-bind (vars3 vals3 store-vars3 writer-form3 reader-form3)
	  (get-setf-expansion next-entries)
	(let ((av (gensym))
	      (new (car store-vars))
	      (new2 (car store-vars2))
	      (new3 (car store-vars3))
	      (r (gensym)))
	  (when (cdr store-vars) (error "can't expand this"))
	  (when (cdr store-vars2) (error "can't expand this"))
	  (when (cdr store-vars3) (error "can't expand this"))
	  `(let* ((,av ,analyser)
		  ,@(mapcar #'list vars vals)
		  (,new ,reader-form)
		  ,@(mapcar #'list vars2 vals2)
		  (,new2 ,reader-form2)
		  ,@(mapcar #'list vars3 vals3)
		  (,new3 ,reader-form3))
	     (multiple-value-bind (,r ,new ,new2 ,new3)
		 (make-loop ,av ,new ,new2 ,new3)
	       ,writer-form
	       ,writer-form2
	       ,writer-form3
	       ,r)))))))

(defun find-independent-groups (analyser entries independent-groups &optional (ignore nil ignorep))
  (declare (cl:type analyser analyser)
	   (ignore analyser)
	   (cl:type list entries) ;block set; not modified
	   (cl:type hash-table independent-groups) ;maps block to block set; modified destructively
	   (cl:type list ignore) ;block set; not modified
	   )
  (let ((ownership (make-hash-table))
	(to-invalidate (make-deque)) ;we reuse this (so we don't need to reallocate it each time).
				     ;this is only used by invalidate-with-children
	(queue (make-deque)) ;queue of blocks
	)
    (declare (cl:type hash-table ownership)) ;maps block to block -- each block to its owner
    (flet ((invalidate-with-children (new)
	     (declare (cl:type block new))
	     (assert (zerop (dq-length to-invalidate)))
	     (dq-push-back to-invalidate new)
	     (loop while (plusp (dq-length to-invalidate))
		do (let* ((invalidatee (dq-pop-front to-invalidate))
			  (owner (gethash invalidatee ownership)))
		     (multiple-value-bind (val pres) (gethash owner independent-groups)
		       (when pres (setf (gethash owner independent-groups)
					(remove invalidatee val))))
		     (when owner
		       (setf (gethash invalidatee ownership) nil)
		       (loop for (target . nil) in (branches-out invalidatee)
			  for target-owner = (gethash target ownership)
			  when target-owner do (dq-push-back to-invalidate target)))))))

      (loop for entry in entries
	 do (setf (gethash entry ownership) entry)
	   (push entry (gethash entry independent-groups nil))
	   (dq-push-back queue entry))
      (loop while (plusp (dq-length queue))
	 do (prog* ((current (dq-pop-front queue))
		    (owner (gethash current ownership)))
	       (unless owner (return)) ;skip if it's already been invalidated
	       (loop for (new . nil) in (branches-out current)
		  do (multiple-value-bind (new-owner pres) (gethash new ownership)
		       (if pres
			   (when new-owner
			     (unless (eq owner new-owner)
			       ;; we reached this from two locations so invalidate this and all
			       ;; reachable from it
			       (invalidate-with-children new)))
			   (progn
			     ;;new node
			     (setf (gethash new ownership) owner)
			     (pushnew new (gethash owner independent-groups))
			     (dq-push-back queue new)))))))

      (loop for entry in entries
	 with to-invalidate = (make-deque)
	 for current-group = (gethash entry independent-groups)
	 do (assert (zerop (dq-length to-invalidate)))
	   (loop for child in current-group
	      do (loop for parent in (branches-in child)
		    unless (or (and ignorep ignore (member parent ignore)) ;do we ignore?
			       (eq (gethash parent ownership) (gethash child ownership)))
		    do (dq-push-back to-invalidate child)))
	   (loop while (plusp (dq-length to-invalidate))
	      do (let ((invalidatee (dq-pop-front to-invalidate)))
		   (invalidate-with-children invalidatee))))
      (maphash (lambda (k v) (unless v (remhash k independent-groups)))
	       independent-groups))))

(defun assoc-value (key list &optional default)
  "like gethash but for alists"
  (let ((x (assoc key list)))
    (values (if x (cdr x) default) (not (null x)))))
(define-setf-expander assoc-value (key list &optional (default nil defaultp) &environment env)
  (let ((key-dummy (gensym "KEY"))
	(default-dummy (gensym "DEFAULT"))
	(list-dummy (gensym "LIST"))
	(cons-dummy (gensym "CONS"))
	(cons-dummy2 (gensym "NEWCONS"))
	(store-var (gensym "STORE"))
	(defaultp (if defaultp (not (constantp default env)) nil)))
    (multiple-value-bind (dummies vals store-vars list-writer list-reader)
	(get-setf-expansion list env)
      (values `(,key-dummy ,@dummies ,list-dummy ,cons-dummy ,@(if defaultp (list default-dummy)))
	      `(,key ,@vals ,list-reader (if ,list-dummy (assoc ,key-dummy ,list-dummy))
		     ,@(if defaultp (list default)))
	      (list store-var)
	      `(if ,cons-dummy
		   (progn (rplacd ,cons-dummy ,store-var) ,store-var)
		   (let ((,cons-dummy2 (cons ,key-dummy ,store-var)))
		     (multiple-value-bind
			   (,@store-vars)
			 (values (cons ,cons-dummy2 ,list-dummy))
		       ,list-writer)
		     (setq ,list-dummy ,list-reader)
		     (setq ,cons-dummy ,cons-dummy2)
		     ,store-var))
	      `(if ,cons-dummy (cdr ,cons-dummy) ,(if defaultp default-dummy default))))))

;;; we use this for hash-table iteration by maphash
;;; when we read we just use the var (i.e. the second parameter to the inner function)
;;; when we write we use (setf gethash)
(defmacro var-holding-hash-value (var key hashtable)
  (declare (ignore key hashtable))
  var)
(define-setf-expander var-holding-hash-value (var key hashtable)
  (let ((ht (make-symbol "HASHTABLE"))
	(k (make-symbol "KEY"))
	(store (make-symbol "NEW")))
    (values (list k ht) (list key hashtable)
	    (list store)
	    `(setf ,var ,store (gethash ,k ,ht) ,store)
	    var)))

(defun make-multiple (analyser blocks entries independent-groups prev next-entries)
  (declare (cl:type analyser analyser)
	   (cl:type list blocks entries next-entries) ;sets of blocks; blocks, next-entries is modified
	   (cl:type hash-table independent-groups) ;maps blocks to sets of blocks; destructively modified
	   (cl:type shape prev))
  (let ((fused (simple-p prev))
	(multiple (make-instance 'multiple-shape))
	(current-entries) ;a set of blocks
	)
    (notice analyser multiple)
    (maphash
     (lambda (current-entry current-blocksv)
       (symbol-macrolet ((current-blocks
			  (var-holding-hash-value current-blocksv current-entry independent-groups)))
	 (setf current-entries (list current-entry))
	 (loop for current-inner in current-blocks
	    do (setf blocks (remove current-inner blocks))
	    ;; want to do this but solipsize can modify (branches-out current-inner)
	    ;; (loop for (current-target . nil) in (branches-out current-inner)
	    ;;    unless (member current-target current-blocks)
	    ;;    do (pushnew current-target next-entries)
 	    ;; 	 (solipsize analyser current-target :break multiple current-blocks))
	      (loop with seen-blocks = (make-hash-table)
		 while (loop for (current-target . nil) in (branches-out current-inner)
			  unless (gethash current-target seen-blocks)
			  do (setf (gethash current-target seen-blocks) t)
			    (unless (member current-target current-blocks)
			      (pushnew current-target next-entries)
			      (solipsize analyser current-target :break multiple current-blocks)
			      (return t)))))
	 (setf (assoc-value (id current-entry) (inner-map multiple))
	       (process& analyser current-blocks current-entries nil))
	 (unless fused
	   (setf (checked-multiple-entry-p current-entry) t))))
     independent-groups)
    (loop for entry in entries
       unless (nth-value 1 (gethash entry independent-groups))
       do (pushnew entry next-entries))
    (when (>= (length (inner-map multiple)) 10) ;we can choose the condition to use a switch
      (setf (use-switch multiple) t)
      (incf (breaks multiple)))
    (values multiple blocks next-entries)))

(defmacro make-multiple& (analyser blocks entries independent-groups prev next-entries &environment env)
  (multiple-value-bind (vars vals store-vars writer-form reader-form)
      (get-setf-expansion blocks env)
    (multiple-value-bind (vars2 vals2 store-vars2 writer-form2 reader-form2)
	(get-setf-expansion next-entries)
      (let ((av (gensym))
	    (ev (gensym))
	    (iv (gensym))
	    (pv (gensym))
	    (new (car store-vars))
	    (new2 (car store-vars2))
	    (r (gensym)))
	(when (cdr store-vars) (error "can't expand this"))
	(when (cdr store-vars2) (error "can't expand this"))
	`(let* ((,av ,analyser)
		,@(mapcar #'list vars vals)
		(,new ,reader-form)
		(,ev ,entries)
		(,iv ,independent-groups)
		(,pv ,prev)
		,@(mapcar #'list vars2 vals2)
		(,new2 ,reader-form2))
	   (multiple-value-bind (,r ,new ,new2)
	       (make-multiple ,av ,new ,ev ,iv ,pv ,new2)
	     ,writer-form
	     ,writer-form2
	     ,r))))))

;;;  Shape *Process(analyser, BlockSet &Blocks, BlockSet& InitialEntries, Shape *Prev) {

(defun process (analyser blocks initial-entries previous)
  (declare (cl:type analyser analyser)
	   (cl:type list blocks initial-entries) ;sets of blocks
	   (cl:type (or shape null) previous))
  ;; returns (values shape new-blocks new-initial-entries)
  (let ((entries initial-entries) ;block set
	(initial t)
	(next-entries) ;block set
	(ret nil)) ;shape
    (declare (cl:type list entries next-entries)
	     (cl:type (or null shape) ret))
    (macrolet ((make (form)
		 (let ((s (symbol-name (car form))))
		   (assert (char= (aref s (1- (length s))) #\&) () "not using an make-x& macro!"))
		 (let ((temp (gensym)))
		   `(let ((,temp ,form))
		      (declare (cl:type (or shape null) ,temp))
		      (when initial
			(setf initial-entries entries
			      initial nil))
		      (when previous (setf (next previous) ,temp))
		      (unless ret (setf ret ,temp))
		      (unless next-entries (return ret))
		      (setf previous ,temp
			    entries next-entries)
		      (go continue)))))
      (let
	  ((return
	     (prog ((independent-groups (make-hash-table)))	;while(1)
	      continue
	      (setf next-entries nil)
	      (when (null entries) (return ret))
	      (when (= 1 (length entries))
		(let ((current (first entries)))
		  (declare (cl:type block current))
		  (when (relooper-emulate (parent analyser))
		    (make (make-emulated& analyser blocks current next-entries)))
		  (when (null (branches-in current))
		    ;; one entry; no looping
		    (make (make-simple& analyser blocks current next-entries)))
		  ;; one entry; looping -> loop
		  (make (make-loop& analyser blocks entries next-entries))))
	      (clrhash independent-groups)
	      (find-independent-groups analyser entries independent-groups)
	      (when (plusp (hash-table-count independent-groups))
		(maphash
		 (lambda (entry group)
		   (declare (cl:type block entry)
			    (cl:type list group))
		   (loop for origin in (branches-in entry)
		      unless (member origin group)
		      do (remhash entry independent-groups)
			(return)))
		 independent-groups)
		(when (= 2 (hash-table-count independent-groups))
		  (prog (small-entry small-group
			 large-entry
			 (dead-end t))
		     (with-hash-table-iterator (iter independent-groups)
		       (multiple-value-bind (more? key val) (iter)
			 (assert more?)
			 (multiple-value-bind (more? key2 val2) (iter)
			   (assert more?)
			   (let ((size (length val)) (size2 (length val2)))
			     (when (= size size2) (return))
			     (if (< size size2)
				 (setf small-entry key
				       small-group val
				       large-entry key2)
				 (setf small-entry key2
				       small-group val2
				       large-entry key)))
			   (assert (null (iter))))))
		     (loop named outer
			for current in small-group
			do (loop for (target . nil) in (branches-out current)
			      unless (member target small-group)
			      do (setf dead-end nil)
				(return-from outer)))
		     (when dead-end
		       (remhash large-entry independent-groups))))
		(when (plusp (hash-table-count independent-groups))
		  ;; some groups removeable ==> multiple
		  (make (make-multiple& analyser blocks entries independent-groups
					previous next-entries))))
	      ;; no independent groups, must be loopable ==> loop
	      (make (make-loop& analyser blocks entries next-entries))
	      ;; the above ensures we go back to the start of the loop
	      )))
	(values return blocks initial-entries)))))




(defclass post-optimiser (relooper-recursor)
  ((closure :initform nil
	    :initarg :closure
	    :accessor closure)))

(defmacro recurse-multiple (shape var form)
  `(loop for (nil . ,var) in (inner-map ,shape)
      do ,form))
;;; TODO: recurse-loop
;;; TODO: recurse
;;; should probably make these more lisp-like macros than the C++
;;; TODO: shape-switch perhaps should just use typecase

(declaim (inline consnew))
(defun consnew (x list &key (test 'eql))
  "a cross between pushnew and cons"
  (if (member x list :test test)
      list
      (cons x list)))

;;; note: returns new out
(defun follow-natural-flow (post s out)
  (declare (cl:type post-optimiser post)
	   (cl:type (or shape null) s)
	   (cl:type list out)) ;block set
  (etypecase s
    (null out)
    (simple-shape (consnew (inner s) out))
    (multiple-shape
     (let ((newout
	    (loop for (nil . x) in (inner-map s)
	       for y = out then (follow-natural-flow post x out)
	       finally (return y))))
       (follow-natural-flow post (next s) newout)))
    (loop-shape
     (follow-natural-flow post (inner s) out))))
;;; maybe todo: follow-natural-flow&

(defun find-naturals (post root &optional otherwise)
  (declare (cl:type post-optimiser post)
	   (cl:type shape root)
	   (cl:type (or shape null) otherwise))
  (if (next root)
      (progn
	(setf (natural root) (next root))
	(find-naturals post (next root) otherwise))
      (setf (natural root) otherwise))
  (etypecase root
    (simple-shape)
    (multiple-shape
     (loop for (nil . x) in (inner-map root)
	do (find-naturals post x (natural root))))
    (loop-shape
     (find-naturals post (inner root) (inner root)))))

(defun remove-unneeded-flows (post root &optional natural last-loop (depth 0))
  (declare (cl:type post-optimiser post)
	   (optimize (debug 3))
	   (cl:type shape root)
	   (cl:type (or shape null) natural)
	   (cl:type (or loop-shape null) last-loop)
	   (cl:type fixnum depth))
  (let ((natural-blocks (follow-natural-flow post natural '()))
	(next root))
    (loop while next
       do (setf root next
		next nil)
	 (typecase root
	   (simple-shape
	    (when (branch-var (inner root)) (setf last-loop nil))
	    (if (next root)
		(progn
		  (when (and (not (branch-var (inner root)))
			     (= 2 (length (processed-branches-out (inner root))))
			     (< depth 20))
		    (let (found abort)
		      (loop for (target . details) of-type (block . branch)
			      in (processed-branches-out (inner root))
			 if (eq (type details) :break)
			 do (setf found t)
			   (unless (member target natural-blocks) (setf abort t))
			 else do (unless (eq (type details) :direct)
				   (setf abort t)))
		      (when (and found (not abort))
			(loop for (target . details) of-type (block . branch)
			        in (processed-branches-out (inner root))
			   if (eq (type details) :break)
			   do (setf (type details) :direct)
			     (let ((m (ancestor details)))
			       (when (typep m 'multiple-shape)
				 (decf (breaks m))))
			   else do (assert (eq (type details) :direct))
			     (setf (type details) :nested)))
		      (incf depth)))
		  (setf next (next root)))
		(loop for (target . details) of-type (block . branch)
		        in (processed-branches-out (inner root))
		   if (and (not (eq (type details) :direct))
			   (member target natural-blocks))
		   do (setf (type details) :direct)
		     (let ((m (ancestor details)))
		       (when (typep m 'multiple-shape)
			 (decf (breaks m))))
		   else do
		     (when (and (eq (type details) :break)
				last-loop
				(eq (natural last-loop)
				    (natural (ancestor details))))
		       (setf (labelled details) nil)
		       (let ((m (ancestor details)))
			 (when (typep m 'multiple-shape)
			   (decf (breaks m))))))))
	   (multiple-shape
	    (loop for (nil . s) in (inner-map root)
	       do (remove-unneeded-flows post s (next root)
					 (if (breaks root)
					     nil last-loop)
					 (1+ depth)))
	    (setf next (next root)))
	   (loop-shape
	    (remove-unneeded-flows post (inner root) (inner root) root (1+ depth))
	    (setf next (next root)))))))

(defun find-labelled-loops (post root)
  (declare (cl:type post-optimiser post)
	   (cl:type (or shape null) root))
  (let ((next root))
    (symbol-macrolet ((loop-stack (closure post)))
      ;; closure acts as a stack so we don't need to set it to anything
      (loop while next
	 do (setf root next
		  next nil)
	   (etypecase root
	     (simple-shape
	      (let ((fused (if (multiple-p (next root)) (next root) nil)))
		(when (and fused (plusp (breaks fused)))
		  (push fused loop-stack))
		(when (branch-var (inner root))
		  (push nil loop-stack))
		(when fused
		  (when (use-switch fused)
		    (push nil loop-stack))
		  (recurse-multiple fused x (find-labelled-loops post x)))
		(loop for (target . details) of-type (block . branch)
		        in (processed-branches-out (inner root))
		   when (member (type details) '(:break :continue))
		   do (assert loop-stack) ;positive size
		     (if (and (not (eq (ancestor details) (car loop-stack)))
			      (labelled details))
			 (setf (labelled (ancestor details)) t)
			 (setf (labelled details) nil)))
		(when (and fused (use-switch fused))
		  (pop loop-stack))
		(when (branch-var (inner root))
		  (pop loop-stack))
		(when (and fused (plusp (breaks fused)))
		  (pop loop-stack))
		(setf next
		      (if fused (next fused) (next root)))))
	     (multiple-shape
	      (when (plusp (breaks root))
		(push root loop-stack))
	      (recurse-multiple root x (find-labelled-loops post x))
	      (when (plusp (breaks root))
		(pop loop-stack))
	      (setf next (next root)))
	     (loop-shape
	      (push root loop-stack)
	      (find-labelled-loops post (inner root))
	      (pop loop-stack)
	      (setf next (next root))))))))

(defun post-process (post root)
  (declare (cl:type post-optimiser post)
	   (cl:type shape root))
  (find-naturals post root)
  (remove-unneeded-flows post root)
  (find-labelled-loops post root))

(defun relooper-calculate (relooper entry)
  (declare (cl:type relooper relooper)
	   (cl:type block entry))
  (let ((pre (make-instance 'pre-optimiser :parent relooper)))
    (find-live pre entry)
    ;; now we set up predecessors of live blocks. perhaps should iterate over live instead
    (loop for i below (dq-length (relooper-blocks relooper))
       for current = (dqref (relooper-blocks relooper) i)
       when (member current (live pre))
       do (loop for (blk . nil) in (branches-out current)
	     do (pushnew current (branches-in blk))))
    (unless (relooper-emulate relooper) (split-dead-ends pre))
    (let ((all-blocks (live pre))
	  (entries (list entry))
	  (analyser (make-instance 'analyser :parent relooper)))
      (let ((root (process analyser all-blocks entries nil)))
	(assert root)
	(let ((post (make-instance 'post-optimiser :parent relooper)))
	  (post-process post root)
	  (setf (relooper-root relooper) root))))))

(defun relooper-render (relooper)
  (declare (cl:type relooper relooper))
  (assert (relooper-root relooper))
  (render-shape (relooper-root relooper) nil))

(defun relooper-calculate-and-render (relooper entry)
  (relooper-calculate relooper entry)
  (relooper-render relooper))

(defun add-block (rl code &optional branch-var)
  (let ((x (make-block code branch-var)))
    (relooper-add-block rl x)
    x))

(defmacro with-relooper ((var &key (label '(gensym)) emulate) &body body)
  "Binds `var' to a relooper object in `body' so that `add-block' (and
thus `add-branch') can be used. body should return a block representing
the entry point. `with-relooper' returns some JS for the blocks and branches."
  (let ((entry-var (gensym)))
    `(let ((*block-id-counter* 1)
	   (*shape-id-counter* 0)
	   (*relooper-label-var* ,label)
	   (,var (make-relooper :emulate ,emulate)))
       (let ((,entry-var (progn ,@body)))
	 (relooper-calculate-and-render ,var ,entry-var)))))
