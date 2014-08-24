(defpackage :dhs-db-connection-pooling
  (:use :cl)
  (:nicknames :dhs-db-connpool)
  (:export #:pool
           #:pool-max-connections
           #:make-connection-pool
           #:allocate-connection
           #:free-connection
           #:free-all-connections
           #:with-pooled))

(in-package :dhs-db-connection-pooling)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *compile-decl* '(optimize (speed 0) (safety 3) (debug 3))))
