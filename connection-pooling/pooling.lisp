(in-package :dhs-db-connection-pooling)

(declaim #.*compile-decl*)

(defclass pooled-connection ()
  ((connection :type dhs-db:connection
               :initarg :connection
               :initform (error "~s was not specified when creating ~s" :connection 'pooled-connection)
               :reader pooled-connection-connection
               :documentation "The underlying connection")
   (active     :initform nil
               :initarg :active
               :accessor pooled-connection-active
               :documentation "Flag that indicates if the connection is in use.")))

(defclass pool ()
  ((connect-args    :type list
                    :initarg :connect-args
                    :initform (error "~s was not specified when creating ~s" :connect-args 'pool)
                    :reader pool-connect-args
                    :documentation "The connect arguments to use when connecting to the database.")
   (db-type         :initarg :db-type
                    :reader pool-db-type
                    :initform (error "~s was not specified when creating ~s" :db-type 'pool))
   (connections     :type list
                    :initform nil
                    :accessor pool-active-connections
                    :documentation "A list of all connections.")
   (pool-lock       :initform (bordeaux-threads:make-lock "Connection pool mutex")
                    :reader pool-lock
                    :documentation "Mutex used to protect the pool instance")
   (max-connections :type integer
                    :accessor pool-max-connections
                    :initform 2
                    :documentation "The maximum number of free connections.")))

(defmethod print-object ((obj pool) out)
  (print-unreadable-object (obj out :type t :identity t)
    (bordeaux-threads:with-lock-held ((pool-lock obj))
      (let* ((n        (length (pool-active-connections obj)))
             (num-free (count nil (pool-active-connections obj) :key #'pooled-connection-active)))
      (format out "TOTAL ~a ACTIVE ~a FREE ~a" n (- n num-free) num-free)))))

(defun make-connection-pool (connect-args database-type)
  (make-instance 'pool :connect-args connect-args :db-type database-type))

(defun allocate-connection (pool)
  (bordeaux-threads:with-lock-held ((pool-lock pool))
    (with-slots (connections) pool
      (let ((conn (find nil connections :key #'pooled-connection-active)))
        (cond (conn
               (progn
                 (setf (pooled-connection-active conn) t)
                 (pooled-connection-connection conn)))
              (t
               (let ((new-connection (dhs-db:connect (pool-connect-args pool) (pool-db-type pool))))
                 (push (make-instance 'pooled-connection :connection new-connection :active t) connections)
                 new-connection)))))))

(defun free-connection (conn pool)
  (when (dhs-db:in-transaction-p conn)
    (error "Trying to free a connection which has an active transaction"))
  (bordeaux-threads:with-lock-held ((pool-lock pool))
    (with-slots (connections) pool
      (let ((backend (find conn connections :key #'pooled-connection-connection)))
        (unless backend
          (error "Trying to free a connection from the wrong pool"))
        (cond ((>= (count nil connections :key #'pooled-connection-active) (pool-max-connections pool))
               (setf connections (remove backend connections))
               (dhs-db:disconnect conn))
              (t
               (setf (pooled-connection-active backend) nil)))
        ;; Make sure no weird return values escape
        nil))))

(defun free-all-connections (pool)
  (bordeaux-threads:with-lock-held ((pool-lock pool))
    (with-slots (connections) pool
      (dolist (conn connections)
        (dhs-db:disconnect (pooled-connection-connection conn)))
      (setf connections nil))))

(defmacro with-pooled ((connection pool) &body body)
  (let ((connection-value (gensym "CONNECTION"))
        (pool-value (gensym "POOL")))
    `(let (,connection-value
           (,pool-value ,pool))
       (unwind-protect
            (progn
              (setq ,connection-value (allocate-connection ,pool-value))
              (let ((,connection ,connection-value))
                ,@body))
         (when ,connection-value
           (free-connection ,connection-value ,pool-value))))))
