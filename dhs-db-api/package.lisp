(defpackage :dhs-db
  (:use :cl)
  (:export #:connect
           #:disconnect
           #:select

           #:prepare-statement
           #:execute-prepared-select
           #:execute-prepared

           #:execute
           #:begin-transaction
           #:commit-transaction
           #:rollback-transaction
           #:in-transaction-p
           #:prepare-statement
           #:close-prepared-statement

           #:connection
           #:prepared-statement

           #:db-error
           #:transaction-active
           #:no-transaction-active
           #:prepared-statement-error

           #:with-transaction
           #:ensure-transaction
           #:with-prepared-statement

           ;; Timestamp functions
           #:database-timestamp
           #:database-date
           #:database-time
           #:make-database-timestamp
           #:make-database-date
           #:make-database-time
           #:format-date
           #:timestamp-value

           #:build-select-col

           ;; Default prepared statement implementation
           #:default-prepare-statement
           #:request-rollback-tran))

(in-package :dhs-db)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *compile-decl* '(optimize (speed 0) (safety 3) (debug 3))))
