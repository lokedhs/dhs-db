(in-package :dhs-db)

(declaim #.*compile-decl*)

(defclass prepared-statement ()
  ()
  (:documentation "Base superclass for prepared statements"))

(defclass connection ()
  ()
  (:documentation "Superclass for all database connections"))

(define-condition db-error (error)
  ()
  (:documentation "Superclass for all errors in the api"))

(define-condition missing-db-access-impl (db-error)
  ((database-type :initarg :database-type
                  :reader missing-db-access-impl-database-type
                  :documentation "The name of the database type that was not found"))
  (:report (lambda (condition out)
             (format out "Database type ~a is not registered"
                     (missing-db-access-impl-database-type condition))))
  (:documentation "This error is signalled when trying to connect to an unregistered database implementation"))

(define-condition no-transaction-active (db-error)
  ()
  (:report "No transaction is active")
  (:documentation "This error is signalled when an attempt is made to commit or rollback a
transaction when no transaction is active"))

(define-condition transaction-active (db-error)
  ()
  (:report "A transaction is already active on this connection")
  (:documentation "This error is signalled when an attempt is made to start a transaction
when a transaction is already active."))

(define-condition prepared-statement-error (db-error)
  ()
  (:report "Prepared statement failed")
  (:documentation "Error that is signalled when there is an error with prepared statements"))

(defgeneric connect (connect-params database-type)
  (:documentation "Connect to the database implementation given as DATABASE-TYPE. The interpretation
of CONNECT-PARAMS depends on the type."))

(defgeneric disconnect (connection)
  (:documentation "Disconnect from the database. CONNECTION should be a connection object
previously returned from CONNECT"))

(defgeneric prepare-statement (connection statement-specifier)
  (:documentation "Prepare an SQL statement"))

(defgeneric close-prepared-statement (statement)
  (:documentation "Closes a prepared statement. Failure to call this may cause
leaks in some backen implementations."))

(defgeneric execute-prepared-select (prepared-statement args &key result-types)
  (:documentation "Execute a previously prepared query.
RESULT-TYPES specifies the type conversion performed on the results."))

(defgeneric execute-prepared (prepared-statement args)
  (:documentation "Execute a previously prepared statement"))

(defgeneric select (connection query &key result-types)
  (:documentation "Run a SELECT query, returning a vector of rows, each row being a vector of the columns.
RESULT-TYPES specifies the type conversion performed on the results."))

(defgeneric execute (connection query)
  (:documentation "Execute an SQL statement, returns the number of rows that were affected."))

(defgeneric begin-transaction (connection)
  (:documentation "Start a transaction on the connection, it must be terminated by either a
COMMIT-TRANSACTION or ROLLBACK-TRANSACTION. For normal use, consider using WITH-TRANSACTION
instead of this function directly. If the backend supports it, this method should signal
TRANSACTION-ACTIVE if a transaction is already in progress."))

(defgeneric commit-transaction (connection)
  (:documentation "Commits an ongoing transaction. The behaviour is undefined if
a transaction is not currently executing, although backend implementations are
encouraged to signal NO-TRANSACTION-ACTIVE if it is possible to detect such situation."))

(defgeneric in-transaction-p (connection)
  (:documentation "Returns non-nil if a transaction is currently active."))

(defgeneric rollback-transaction (connection)
  (:documentation "Rolls back an ongoing transaction. The behaviour is undefined if
a transaction is not currently executing, although backend implementations are
encouraged to signal NO-TRANSACTION-ACTIVE if it is possible to detect such situation."))

(defgeneric build-select-col (db column-type column-name)
  (:documentation "Certain databases requires conversion expressions for certain data types.
This generic function allows a backend implementation to provide this functionality in
a platform-independent manner."))

(defmethod build-select-col (db column-type column-name)
  "Default implementation of BUILD-SELECT-COL that simply returns its argument."
  column-name)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *rollback-on-transaction-complete* nil
  "If non-NIL at the end of a WITH-TRANSACTION block, the transaction will be rolled back instead of comitted")

(defun request-rollback-tran ()
  (setq *rollback-on-transaction-complete* t))

(defmacro with-transaction ((connection) &body body)
  "Executes BODY within an active transaction. Rolls back the transaction if the scope is exited because of an error."
  (let ((connection-value (gensym "CONNECTION"))
        (running-value (gensym "TRANSACTION-RUNNING"))
        (result (gensym "RESULT")))
    `(let ((,connection-value ,connection)
           (,running-value t)
           (*rollback-on-transaction-complete* nil))
       (begin-transaction ,connection-value)
       (unwind-protect
            (let ((,result (progn ,@body)))
              (if *rollback-on-transaction-complete*
                  (rollback-transaction ,connection-value)
                  (commit-transaction ,connection-value))
              (setq ,running-value nil)
              ,result)
         (when ,running-value
           (rollback-transaction ,connection-value))))))

(defmacro ensure-transaction ((connection) &body body)
  "Ensures that BODY is executed within a transaction. If a transaction is not active,
BODY will be evaluated as if within a call to WITH-TRANSACTION."
  (let ((function (gensym))
        (connection-copy (gensym)))
    `(let ((,connection-copy ,connection)
           (,function #'(lambda () ,@body)))
       (if (in-transaction-p ,connection-copy)
           (funcall ,function)
           (with-transaction (,connection-copy)
             (funcall ,function))))))

(defmacro with-prepared-statement ((statement connection statement-specifier) &body body)
  "Evalues BODY with STATEMENT bound to a prepared statement created based on STATEMENT-SPECIFIER.
After the code is executed, the statement is closed."
  (declare (type symbol statement))
  (let ((s (gensym "STATEMENT")))
    `(let (,s)
       (unwind-protect
            (progn
              (setq ,s (dhs-db:prepare-statement ,connection ,statement-specifier))
              (let ((,statement ,s))
                ,@body))
         (when ,s (dhs-db:close-prepared-statement ,s))))))
