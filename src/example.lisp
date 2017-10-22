(asdf:load-system :cludge)
(defpackage example
  (:use #:cl))

(in-package #:example)

(defparameter *my-env* (make-instance 'js-env:compilation-environment))
(js-syntax-writer:write-js-to-string
 (js-cleavir:compile '(lambda (x) x) *my-env*))
