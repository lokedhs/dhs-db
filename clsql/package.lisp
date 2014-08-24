(defpackage :dhs-db-clsql
  (:use :cl))

(in-package :dhs-db-clsql)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *compile-decl* '(optimize (speed 0) (safety 3) (debug 3))))
