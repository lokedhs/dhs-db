(in-package :dhs-db-dwim)

(defclass dwim-connection (dhs-db:connection)
  ((db :initarg :db
       :reader dwim-connection-db
       :documentation "Holds a reference to the underlying db connection"))
  (:documentation "Wrapper class that holds the connection to the hu.dwim.rdbms connection"))

(defmethod dhs-db:connect ((connect-params t) (database-type (eql :hu.dwim.rdbms)))
  (destructuring-bind (driver &rest connect-spec) connect-params
    (let* ((db (apply #'make-instance driver connect-spec))
           (conn (make-instance 'dwim-connection :db db)))
      ;; It's not really good style to rely on the dwim feature of implicit transactions
      ;; It's probably better to do it in a similar way to how it's handled in the JDBC driver
      (setf hu.dwim.rdbms:*implicit-transaction* :ensure)
      conn)))

(defmethod dhs-db:disconnect ((connection dwim-connection))
  ;;  (hu.dwim.rdbms:
  (format t "WARNING: dwim backend can't currently be closed~%"))

(defmethod dhs-db:select ((connection dwim-connection) query &key result-types)
  (hu.dwim.rdbms:with-database (dwim-connection-db connection)
    (hu.dwim.rdbms:execute query)))

(defmethod dhs-db:execute ((connection dwim-connection) query)
  (hu.dwim.rdbms:with-database (dwim-connection-db connection)
    (hu.dwim.rdbms:execute query)))

(defmethod dhs-db:prepare-statement ((connection dwim-connection) statement-specifier)
  (dhs-db:default-prepare-statement connection statement-specifier))

(defmethod dhs-db:begin-transaction ((connection dwim-connection))
  (hu.dwim.rdbms:with-database (dwim-connection-db connection)
    (when (hu.dwim.rdbms:in-transaction-p)
      (error 'dhs-db:transaction-active))
    (hu.dwim.rdbms:begin)))

(defmethod dhs-db:commit-transaction ((connection dwim-connection))
  (hu.dwim.rdbms:with-database (dwim-connection-db connection)
    (unless (hu.dwim.rdbms:in-transaction-p)
      (error 'dhs-db:no-transaction-active))
    (hu.dwim.rdbms:commit)))

(defmethod dhs-db:rollback-transaction ((connection dwim-connection))
  (hu.dwim.rdbms:with-database (dwim-connection-db connection)
    (unless (hu.dwim.rdbms:in-transaction-p)
      (error 'dhs-db:no-transaction-active))
    (hu.dwim.rdbms:rollback)))
