(defpackage :dhs-db-persist
  (:use :cl)
  (:export #:persisted-db-entry-class
           #:save-instance
           #:update-instance
           #:find-instance-list
           #:find-instance-from-id
           #:retrieve-id
           #:with-active-cache))

(in-package :dhs-db-persist)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *compile-decl* '(optimize (speed 0) (safety 3) (debug 3))))
