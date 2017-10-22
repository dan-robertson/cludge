;;; A testcase for compiling let-over-lambda type code. These are designed
;;; to be compiled one at a time by the JS compiler. These demonstrate
;;; a current issue with cleavir where let (almost) commutes with
;;; tagbody. In particular the following get translated to essentially
;;; the same code in cleavir:
;;; 
;;; (loop for i from 1 to 5 collect (let ((x i)) (lambda () x)))
;;; (let (x) (loop for i from 1 to 5 collect (progn (setf x i) (lambda () x))))


(defun test1 ()
  (tagbody
     (flet ((foo (getter setter)
	      (if (cleavir-primop:eq (cleavir-primop:funcall getter) setter)
		  (go end)
		  (cleavir-primop:funcall setter setter))))
       (tagbody start
	  (let ((x #'foo))
            (let ((getter (lambda () x))
		  (setter (lambda (y) (setq x y))))
	      (foo getter setter)
	      (go start)))))
     end))

(defun test2 ()
  (tagbody
     (let (saved-getter)
       (flet ((foo (getter setter)
		(let ((true-getter
		       (or saved-getter
			   (setf saved-getter getter))))
		  (if (cleavir-primop:eq (cleavir-primop:funcall true-getter) saved-getter)
		      (go end)
		      (cleavir-primop:funcall setter saved-getter)))))
	 (tagbody start
	    (let ((x #'foo))
	      (let ((getter (lambda () x))
		    (setter (lambda (y) (setq x y))))
		(foo getter setter)
		(go start))))))
     end))

(defun test3 ()
  (tagbody
     (let (saved-getter)
       (flet ((foo (getter setter)
		(let ((true-getter
		       (or saved-getter
			   (setf saved-getter getter))))
		  (if (cleavir-primop:eq (cleavir-primop:funcall true-getter) saved-getter)
		      (go end)
		      (cleavir-primop:funcall setter saved-getter)))))
	 (let ((x #'foo))
	   (let ((getter (lambda () x))
		 (setter (lambda (y) (setq x y))))
	     (tagbody start
		(foo getter setter)
		(go start))))))
     end))
