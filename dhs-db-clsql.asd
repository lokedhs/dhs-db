(defpackage :dhs-db-clsql-system
  (:use :cl :asdf))

(in-package :dhs-db-clsql-system)

(defsystem dhs-db-clsql
  :name "dhs-db-clsql"
  :author "Elias Martenson <lokedhs@gmail.com>"
  :license "BSD"
  :description "CLSQL interface to dhs-db"
  :depends-on (:dhs-db :clsql :parse-number)
  :components ((:module :dhs-db-clsql
                        :pathname "clsql"
                        :components ((:file "package")
                                     (:file "dhs-db-clsql" :depends-on ("package"))
                                     (:file "oracle-init" :depends-on ("dhs-db-clsql"))))))
