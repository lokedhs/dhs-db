(in-package :dhs-db-clsql)

(declaim #.*compile-decl*)

(defclass clsql-connection (dhs-db:connection)
  ((db                  :initarg :db
                        :initform (error  "~s required when instantiating ~s" :db 'clsql-connection)
                        :reader clsql-connection-db
                        :documentation "Holds the clsql database instance")
   (clsql-database-type :initarg :clsql-database-type
                        :initform (error "~s required when instantiating ~s" :clsql-database-type 'clsql-connection)
                        :reader clsql-database-type
                        :documentation "The database type used to create the backend"))
  (:documentation "Wrapper class that holds the connection to the database"))

(defgeneric initialise-clsql-db-connection (connection clsql-type)
  (:documentation "Entry point for database-specific initialisation functions.
CONNECTION is the newly created database connection. CLSQL-TYPE is the type
specified as the second argument to CLSQL:CONNECT."))

(defmethod initialise-clsql-db-connection (connection clsql-type)
  "Default empty implementation"
  nil)

(defgeneric clsql-build-select-col (connection underlying-db-type column-type column-name)
  (:documentation "CLSQL-specific version of BUILD-SELECT-COL that can select on database type."))

(defmethod clsql-build-select-col (connection underlying-db-type column-type column-name)
  "Default implementation that simply returns its argument."
  column-name)

(defmethod dhs-db:build-select-col ((connection clsql-connection) column-type column-name)
  "CLSQL-specific implementation that dispatches to an underlying implementation that
is chosen based on which database is actually used."
  (clsql-build-select-col connection (clsql-database-type connection) column-type column-name))

(defmethod dhs-db:connect ((params t) (type (eql :clsql)))
  "Connect using the CLSQL backend. PARAMS is a list of two elements:
The first element being the connection spec to the CLSQL:CONNECT function
and the second element is the database type as passed to the same command."
  (let* ((clsql-db-type (cadr params))
         (connection (make-instance 'clsql-connection
                                    :db (clsql:connect (car params)
                                                       :database-type clsql-db-type
                                                       :pool t)
                                    :clsql-database-type clsql-db-type)))
    (initialise-clsql-db-connection connection clsql-db-type)
    connection))

(defmethod dhs-db:disconnect ((connection clsql-connection))
  (clsql:disconnect :database (clsql-connection-db connection)))

(defgeneric apply-result-type (value typename)
  (:documentation "Converts VALUE to the type specified by TYPENAME. If TYPENAME
is NIL, return VALUE.")
  (:method ((value t) (typename t)) (error "Cannot map objects of type ~s to ~s" (type-of value) typename))
  (:method ((value t)       (typename (eql nil)))        value)
  (:method ((value null)    (typename t))                nil)
  (:method ((value t)       (typename (eql :string)))    (format nil "~a" value))
  (:method ((value integer) (typename (eql :integer)))   value)
  (:method ((value number)  (typename (eql :integer)))   (truncate value))
  (:method ((value number)  (typename (eql :number)))    value)
  (:method ((value string)  (typename (eql :integer)))   (parse-integer value))
  (:method ((value string)  (typename (eql :number)))    (parse-number:parse-number value))
  (:method ((value string)  (typename (eql :timestamp))) (dhs-db:make-database-timestamp value))
  (:method ((value string)  (typename (eql :date)))      (dhs-db:make-database-date value))
  (:method ((value string)  (typename (eql :time)))      (dhs-db:make-database-time value)))

(defun make-result-array (result-row result-types)
  (let ((cols (length result-row)))
    (cond (result-types
           (progn
             (when (/= (length result-types) cols)
               (error "Argument ~s length ~d does not match column length ~d"
                      :result-types (length result-types) cols))
             (map 'vector #'apply-result-type result-row result-types)))
          (t
           (make-array cols :initial-contents result-row)))))

(defmethod dhs-db:select ((connection clsql-connection) query &key result-types)
  (let* ((query-result (clsql:query query :database (clsql-connection-db connection) :field-names nil)))
    (map-into (make-array (length query-result) :element-type 'list :initial-element nil)
              #'(lambda (element) (make-result-array element result-types))
              query-result)))

(defmethod dhs-db:execute ((connection clsql-connection) query)
  (clsql:execute-command query :database (clsql-connection-db connection)))

(defmethod dhs-db:prepare-statement ((connection clsql-connection) statement-specifier)
  (dhs-db:default-prepare-statement connection statement-specifier))

(defmethod dhs-db:begin-transaction ((connection clsql-connection))
  (with-slots (db) connection
    (when (clsql:in-transaction-p :database db)
      (error 'dhs-db:transaction-active))
    (clsql:start-transaction :database db)))

(defmethod dhs-db:commit-transaction ((connection clsql-connection))
  (with-slots (db) connection
    (unless (clsql:in-transaction-p :database db)
      (error 'dhs-db:no-transaction-active))
    (clsql:commit :database db)))

(defmethod dhs-db:rollback-transaction ((connection clsql-connection))
  (with-slots (db) connection
    (unless (clsql:in-transaction-p :database db)
      (error 'dhs-db:no-transaction-active))
    (clsql:rollback :database db)))

(defmethod dhs-db:in-transaction-p ((connection clsql-connection))
  (clsql:in-transaction-p :database (clsql-connection-db connection)))
