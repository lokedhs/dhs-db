(in-package :dhs-db)

(declaim #.*compile-decl*)

(define-condition operation-not-implemented (db-error)
  ((name :initarg :name
         :reader operation-not-implemented-name
         :documentation "The name of the method that was not implemented"))
  (:report (lambda (condition out)
             (format out "The operation ~a is not implemented"
                     (operation-not-implemented-name condition))))
  (:documentation "This error is signalled when there is no implementation for a specific method"))

(defmethod initialize-instance :after ((connection connection) &rest initargs)
  (format t "initialising:~s~%" initargs))

(defmethod connect ((params t) (type t))
  "Default implementation of CONNECT method"
  (error 'missing-db-access-impl :database-type type))

(defmethod disconnect ((connection connection))
  (error 'operation-not-implemented :name 'disconnect))

(defmethod select ((connection connection) query &key result-types)
  (error 'operation-not-implemented :name 'select))

(defmethod execute ((connection connection) query)
  (error 'operation-not-implemented :name 'execute))

(defmethod begin-transaction ((connection connection))
  (error 'operation-not-implemented :name 'begin-transaction))

(defmethod commit-transaction ((connection connection))
  (error 'operation-not-implemented :name 'commit-transaction))

(defmethod rollback-transaction ((connection connection))
  (error 'operation-not-implemented :name 'rollback-transaction))
