(cl:in-package #:js-relooper)

;;; some classes

(defclass branch ()
  ((ancestor :initform nil
	     :initarg :ancestor
	     :accessor ancestor
	     :type (or null shape)
	     :documentation "If non-nil this shape is the relevant one for purposes
of getting to the target block. We break or continue on it.")
   (type :initarg :type
	 :accessor type
	 :type (member :direct :break :continue :nested)
	 :documentation "If ancestor is non-nil this tells us how to get to it,
i.e. break or continue.")
   (labelled :initform t
	     :initarg :labelled
	     :accessor labelled
	     :type boolean
	     :documentation "If breaking/continuing, whether we need to use a label.")
   (condition :initform nil
	      :initarg :condition
	      :accessor condition
	      :documentation "JS for the condition that we take this branch
for example, (js:=== label 10). If this is NIL then this
is the default choice.")
   (code :initform nil
	 :initarg :code
	 :accessor code
	 :documentation "if non-NIL then some JS to run directly before we
take this branch")))

(defclass block ()
  ((branches-out :initform nil ;??? alist should be map taking block->branch
		 :initarg :branches-out
		 :accessor branches-out)
   (branches-in :initform nil ;??? list? should be a set of branch
		:initarg :branches-in
		:accessor branches-in)
   (processed-branches-out :initform nil ;alist; should be map taking block->branch
			   :initarg :processed-branches-out
			   :accessor processed-branches-out)
   (processed-branches-in :initform nil ;??? list? should be a set of branch
			  :initarg :processed-branches-in
			  :accessor processed-branches-in)
   (parent :initform nil
	   :initarg :parent
	   :accessor parent
	   :type (or null shape)
	   :documentation "The shape the block is directly inside")
   (id :initform 0
       :initarg :id
       :accessor id
       :type fixnum
       :documentation "An ID unique to each logical branch (i.e. copies have the same ID)")
   (code :initform nil
	 :initarg :code
	 :accessor code)
   (branch-var :initform nil
	       :initarg :branch-var
	       :accessor branch-var
	       :documentation "A symbol for a JS variable whose value determines
where we go. If non-NIL we emit a switch on it.")
   (checked-multiple-entry :initform nil
			   :initarg :checked-multiple-entry
			   :accessor checked-multiple-entry-p
			   :type boolean
			   :documentation "If true, this block is a multiple entry, so to
reach it a label variable must be set.")))

(defclass shape ()
  ((id :initform 0
	:initarg :id
	:accessor id
	:type fixnum)
   (next :initform nil
	 :initarg :next
	 :accessor next
	 :type (or null shape)
	 :documentation "The shape we go to next")
   (natural :initform nil
	    :initarg :natural
	    :accessor natural
	    :type (or null shape)
	    :documentation "The shape control naturally flows to. If we have a NEXT
then this is the same")))

(defclass simple-shape (shape)
  ((inner :initform nil
	  :initarg :inner
	  :accessor inner
	  :type (or null block)))
  (:documentation "No control flow at all, just instructions."))

(defclass labelled-shape (shape)
  ((labelled :initform nil
	     :initarg :labelled
	     :accessor labelled
	     :type boolean
	     :documentation "If we have a loop, whether it needs to be labelled"))
  (:documentation "A shape that may be implemented as a labelled loop"))

(defclass multiple-shape (labelled-shape)
  ((inner-map :initform nil ;??? alist; should be map taking entry block id->shape
	      :initarg :inner-map
	      :accessor inner-map)
   (breaks :initform 0
	   :initarg :breaks
	   :accessor breaks
	   :type fixnum)
   (use-switch :initform nil
	       :initarg :use-switch
	       :accessor use-switch
	       :type boolean
	       :documentation "Whether to use a switch or an if-else chain"))
  (:documentation "A shape with more than one entry. If the next
block to be entered is amongst them then we run it.
Otherwise we continue straight to the next block"))

(defclass loop-shape (labelled-shape)
  ((inner :initform nil
	   :initarg :inner
	   :accessor inner
	   :type (or null shape)))
  (:documentation "An infinite loop"))

(defclass emulated-shape (labelled-shape)
  ((entry :initform nil
	  :initarg :entry
	  :accessor entry
	  :type (or null block))
   (blocks :initform nil ;?? list; (set of blocks)
	   :initarg :blocks
	   :accessor blocks))
  (:documentation "Control flow is managed/emulated by a switch in a loop."))

(defgeneric simple-p (shape)
  (:method (shape) nil)
  (:method ((shape simple-shape)) t))
(defgeneric multiple-p (shape)
  (:method (shape) nil)
  (:method ((shape multiple-shape)) t))
(defgeneric loop-p (shape)
  (:method (shape) nil)
  (:method ((shape loop-shape)) t))
(defgeneric labelled-p (shape)
  (:method (shape) nil)
  (:method ((shape labelled-shape)) t))
(defgeneric emulated-p (shape)
  (:method (shape) nil)
  (:method ((shape emulated-shape)) t))
(defgeneric shape-type (shape)
  (:method (shape) nil)
  (:method ((shape simple-shape)) :simple)
  (:method ((shape multiple-shape)) :multiple)
  (:method ((shape loop-shape)) :loop)
  (:method ((shape emulated-shape)) :emulated))



