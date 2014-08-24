(in-package :dhs-db-clsql)

(declaim #.*compile-decl*)

(defmethod initialise-clsql-db-connection :after (connection (clsql-type (eql :oracle)))
  (dhs-db:execute connection "alter session set nls_timestamp_format = 'YYYY-MM-DD HH24:MI:SS'"))

(defmethod clsql-build-select-col (connection (underlying-db-type (eql :oracle)) (column-type (eql :timestamp)) column-name)
  (format nil "to_char(~a, 'YYYY-MM-DD HH24:MI:SS')" column-name))

(defmethod clsql-build-select-col (connection (underlying-db-type (eql :oracle)) (column-type (eql :date)) column-name)
  (format nil "to_char(~a, 'YYYY-MM-DD')" column-name))

(defmethod clsql-build-select-col (connection (underlying-db-type (eql :oracle)) (column-type (eql :time)) column-name)
  (format nil "to_char(~a, 'HH24:MI:SS')" column-name))
