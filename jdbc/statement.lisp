(in-package :dhs-db-jdbc)

(defclass jdbc-prepared-statement (dhs-db:prepared-statement)
  ((connection     :initarg :connection
                   :reader jdbc-prepared-statement-connection
                   :documentation "The database connection that belongs to this statement")
   (jdbc-statement :initarg :jdbc-statement
                   :reader jdbc-statement
                   :documentation "The underlying PreparedStatement instance")
   (arg-map        :initarg :arg-map
                   :reader arg-map
                   :documentation "An alist mapping the argument positions to JDBC positions"))
  (:documentation "Prepared statement implementation for the JDBC backend"))

(define-condition prepared-statement-too-many-arguments (dhs-db:prepared-statement-error)
  ()
  (:report "Too many arguments to query function")
  (:documentation "Error that is signalled when too many arguments are passed to the query function"))

(defun append-to-adjustable-string (s v)
  (declare (type string s)
           (type vector v))
  (loop
     for ch across s
     do (vector-push-extend ch v))
  v)

(defun make-prepared-statement (connection statement-specifier)
  (loop
     with map = nil
     with arg-pos = 0
     with query = (make-array 0 :adjustable t :fill-pointer t)

     for first = t then nil
     for part in statement-specifier

     unless first do (append-to-adjustable-string " " query)

     do (etypecase part
            ((string) (append-to-adjustable-string part query))
            ((integer) (progn (unless (assoc part map) (push (list part (incf arg-pos)) map))
                              (append-to-adjustable-string "?" query))))

     finally (return (make-instance 'jdbc-prepared-statement
                                    :connection connection
                                    :jdbc-statement (call-java-method ("java.sql.Connection"
                                                                       "prepareStatement"
                                                                       "java.lang.String")
                                                                      (jdbc-connection-db connection)
                                                                      (coerce query 'simple-base-string))
                                    :arg-map map))))

(defmethod dhs-db:prepare-statement ((connection jdbc-db-connection) statement-specifier)
  (make-prepared-statement connection statement-specifier))

(defun update-jdbc-args (statement args)
  (declare (type jdbc-prepared-statement statement)
           (type list args))
  (loop
     for arg in args
     for i from 0
     do (let ((e (assoc i (arg-map statement))))
          (unless e
            (error 'prepared-statement-too-many-arguments))
          (call-java-method ("java.sql.PreparedStatement" "setObject" "int" "java.lang.Object")
                            (jdbc-statement statement) (cadr e) arg))))

(defmethod dhs-db:execute-prepared-select ((statement jdbc-prepared-statement) args &key result-types)
  (execute-and-commit ((jdbc-prepared-statement-connection statement))
    (let (resultset)
      (unwind-protect
           (progn
             (update-jdbc-args statement args)
             (setq resultset (call-java-method ("java.sql.PreparedStatement" "executeQuery")
                                               (jdbc-statement statement)))
             (convert-resultset resultset result-types))
        (when resultset (call-java-method ("java.sql.ResultSet" "close") resultset))))))

(defmethod dhs-db:execute-prepared ((statement jdbc-prepared-statement) args)
  (execute-and-commit ((jdbc-prepared-statement-connection statement))
    (update-jdbc-args args)
    (call-java-method ("java.sql.PreparedStatement" "executeUpdate")
                      (jdbc-statement statement))))

(defmethod dhs-db:close-prepared-statement ((statement jdbc-prepared-statement))
  (call-java-method ("java.sql.PreparedStatement" "close")
                    (jdbc-statement statement)))
