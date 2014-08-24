(in-package :dhs-db-jdbc)

(defun java-boolean (value)
  (jfield-raw "java.lang.Boolean" (if value "TRUE" "FALSE")))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun find-method-by-name (class-name method-name &rest argument-types)
    (apply #'jmethod (jclass class-name) method-name (mapcar #'jclass argument-types))))

(defmacro call-java-method ((class-name method-name &rest argument-types) object &rest args)
  (let ((method-descriptor (if (every #'(lambda (v) (typep v 'string)) (list* class-name method-name argument-types))
                               (apply #'find-method-by-name class-name method-name argument-types)
                               (list 'find-method-by-name class-name argument-types))))
    `(jcall ,method-descriptor ,object ,@args)))

(defclass jdbc-db-connection (dhs-db:connection)
  ((db          :initarg :db
                :reader jdbc-connection-db
                :documentation "Holds the reference to the underlying Java Connection object")
   (transaction :initform nil
                :accessor jdbc-connection-transaction
                :documentation "True if a transaction is in progress"))
  (:documentation "Wrapper class that holds the reference to the JDBC connection"))

(defmacro execute-and-commit ((connection) &body body)
  (let ((connection-value (gensym "CONNECTION"))
        (result-value (gensym "RESULT"))
        (committed-value (gensym "SUCCESS")))
    `(let ((,connection-value ,connection)
           (,committed-value nil)
           (,result-value nil))
       (unwind-protect
            (progn
              (setq ,result-value (progn ,@body))
              (unless (jdbc-connection-transaction ,connection-value)
                (call-java-method ("java.sql.Connection" "commit") (jdbc-connection-db ,connection-value)))
              (setq ,committed-value t))
         (when (and (not ,committed-value)
                    (not (jdbc-connection-transaction ,connection-value)))
           (call-java-method ("java.sql.Connection" "rollback") (jdbc-connection-db ,connection-value))))
       ,result-value)))

(defmethod dhs-db:connect ((params t) (type (eql :jdbc)))
  (destructuring-bind (url user password) params
      (let (connection
            initialised
            wrapper)
        (unwind-protect
             (progn
               (setq connection (call-java-method ("java.sql.DriverManager" "getConnection"
                                                                            "java.lang.String"
                                                                            "java.lang.String"
                                                                            "java.lang.String")
                                                  nil url user password))
               (call-java-method ("java.sql.Connection" "setAutoCommit" "boolean") connection (java-boolean nil))
               (setq wrapper (make-instance 'jdbc-db-connection :db connection))
               (setq initialised t)
               wrapper)
          (when (and connection (not initialised))
            (call-java-method ("java.sql.Connection" "close") connection))))))

(defmethod dhs-db:disconnect ((connection jdbc-db-connection))
  (when (jdbc-connection-transaction connection)
    (rollback connection)))

(defun convert-java-value (value)
  (cond ((java-object-p value)
         (let ((bignumclass (jclass "java.math.BigDecimal")))
           (if (call-java-method ("java.lang.Class" "isInstance" "java.lang.Object") bignumclass value)
               (call-java-method ("java.math.BigDecimal" "doubleValue") value)
               (error "Can't convert java type: ~s" value))))
        (t
         value)))

(defun convert-value (resultset column type)
  (let ((jdbc-col (1+ column)))
    (ecase type
      ((nil)      (convert-java-value (call-java-method ("java.sql.ResultSet" "getObject" "int")
                                                       resultset jdbc-col)))
      ((:string)  (call-java-method ("java.sql.ResultSet" "getString" "int") resultset jdbc-col))
      ((:integer) (call-java-method ("java.sql.ResultSet" "getInt" "int") resultset jdbc-col))
      ((:number)  (call-java-method ("java.sql.ResultSet" "getDouble" "int") resultset jdbc-col)))))

(defun convert-resultset (resultset result-types)
  (let* ((meta-data (call-java-method ("java.sql.ResultSet" "getMetaData") resultset))
         (column-count (call-java-method ("java.sql.ResultSetMetaData" "getColumnCount") meta-data)))
    (loop
       with result = (make-array 0 :adjustable t :fill-pointer t)
       while (call-java-method ("java.sql.ResultSet" "next") resultset)
       do (vector-push-extend (loop
                                 with row-result = (make-array column-count :initial-element nil)
                                 with type = result-types
                                 for i from 0 below column-count
                                 do (setf (aref row-result i)
                                          (convert-value resultset i (car type)))
                                 when result-types do (setq type (cdr type))
                                 finally (return row-result))
                              result)
       finally (return result))))

(defmacro with-statement ((connection statement-sym) &body body)
  (declare (type symbol statement-sym))
  (let ((connection-value (gensym "CONNECTION"))
        (statement (gensym "STATEMENT")))
    `(let ((,connection-value ,connection)
           (,statement nil))
       (unwind-protect
            (progn
              (setq ,statement (call-java-method ("java.sql.Connection" "createStatement")
                                                 (jdbc-connection-db ,connection-value)))
              (let ((,statement-sym ,statement))
                ,@body))
         (when ,statement (call-java-method ("java.sql.Statement" "close") ,statement))))))

(defmacro execute-and-commit-with-statement ((connection statement-sym) &body body)
  (declare (type symbol statement-sym))
  (let ((connection-value (gensym "CONNECTION")))
    `(let ((,connection-value ,connection))
       (execute-and-commit (,connection-value)
         (with-statement (,connection-value ,statement-sym)
           ,@body)))))

(defmethod dhs-db:select ((connection jdbc-db-connection) query &key result-types)
  (execute-and-commit-with-statement (connection java-sql-statement)
    (let (resultset)
      (unwind-protect
           (progn
             (setq resultset (call-java-method ("java.sql.Statement" "executeQuery" "java.lang.String")
                                               java-sql-statement query))
             (convert-resultset resultset result-types))
        (when resultset (call-java-method ("java.sql.ResultSet" "close") resultset))))))

(defmethod dhs-db:execute ((connection jdbc-db-connection) query)
  (execute-and-commit-with-statement (connection java-sql-statement)
    (call-java-method ("java.sql.Statement" "executeUpdate" "java.lang.String")
                      java-sql-statement query)))

(defmethod dhs-db:begin-transaction ((connection jdbc-db-connection))
  (when (jdbc-connection-transaction connection)
    (error 'dhs-db:transaction-active))
  (setf (jdbc-connection-transaction connection) t))

(defmethod dhs-db:commit-transaction ((connection jdbc-db-connection))
  (unless (jdbc-connection-transaction connection)
    (error 'dhs-db:no-transaction-active))
  (call-java-method ("java.sql.Connection" "commit") (jdbc-connection-db connection))
  (setf (jdbc-connection-transaction connection) nil))

(defun rollback (connection)
  (call-java-method ("java.sql.Connection" "rollback") (jdbc-connection-db connection))
  (setf (jdbc-connection-transaction connection) nil))

(defmethod dhs-db:rollback-transaction ((connection jdbc-db-connection))
  (unless (jdbc-connection-transaction connection)
    (error 'dhs-db:no-transaction-active))
  (rollback connection))
