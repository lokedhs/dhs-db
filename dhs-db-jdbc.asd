(defpackage :dhs-db-jdbc-system
  (:use :cl :asdf))

(in-package :dhs-db-jdbc-system)

(defsystem dhs-db-jdbc
  :name "dhs-db-jdbc"
  :author "Elias Martenson <lokedhs@gmail.com>"
  :license "BSD"
  :description "JDBC interface to dhs-db (uses ABCL)"
  :depends-on (:dhs-db)
  :components ((:module :dhs-db-jdbc
                        :pathname "jdbc"
                        :components ((:file "package")
                                     (:file "dhs-db-jdbc" :depends-on ("package"))
                                     (:file "statement" :depends-on ("dhs-db-jdbc"))))))
