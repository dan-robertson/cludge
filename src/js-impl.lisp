;;; This file contains implementations for:
;;; a) various environment macros specialised to the way we compile JS
;;; b) various functions we need implementations of in JS
(cl:in-package #:js-impl)

(defmacro defun (name lambda-list &body body)
  (multiple-value-bind (declarations documentation forms)
      (cleavir-code-utilities:separate-function-body body)
    (let ((truename (if (consp name) (second name) name))
	  (setfdef (if (consp name)
		       'js-special:set-sfdefinition
		       'js-special:set-fdefinition)))
      `(eval-when (:compile-toplevel :load-toplevel :execute)
	 (,setfdef (lambda ,lambda-list
		     ,@declarations
		     ,@(and documentation (list documentation))
		     (block ,truename ,@forms))
		   ',truename)))))


(defmacro in-package (string-designator)
  (let ((p (find-package string-designator)))
    (unless p (error "~a does not designate a package" string-designator))
    `(progn
       (eval-when (:compile-toplevel :load-toplevel)
	 (setf *package* ,p))
       ;; we don't really need this in JS
       #|(eval-when (:execute)
	 (setf *package* (find-package ',string-designator)))
       |#)))


;;; NB. after we define these, we can't use them in our js-impl
;;; This is because this file is EVAL'd, not COMPILE-FILE'd
;;; hence the expansions of these macros try to mess with the JS
;;; environment and not the compilation environment
(defmacro defvar (name &optional (initial-value nil initial-value-p) documentation)
  (declare (ignore documentation))
  `(progn
     (eval-when (:compile-toplevel)
       (setf (sicl-genv:special-variable
	      ',name
	      (load-time-value (sicl-genv:global-environment))
	      nil)
	     nil))
     ,@(when initial-value-p		;we don't bind it unless we have a value
	 (list
	  `(eval-when (:load-toplevel :execute)
	     (unless (js-special:boundp ',name)
	       (js-special:add-global-special ',name)
	       (setq ,name ,initial-value)))))
     ',name))

(defmacro defparameter (&environment env name initial-value &optional documentation)
  (declare (ignore documentation))
  `(progn
     (eval-when (:compile-toplevel)
       (setf (sicl-genv:special-variable ',name ,env nil)
	     nil))
     (eval-when (:load-toplevel :execute)
       (unless (js-special:boundp ',name)
	 (js-special:add-global-special ',name))
       (setq ,name ,initial-value))
     ',name))

(defun set (symbol newvalue)
  (unless (js-special:boundp symbol)
    (js-special:add-global-special symbol))
  (js-special:set-symbol-value symbol newvalue))
