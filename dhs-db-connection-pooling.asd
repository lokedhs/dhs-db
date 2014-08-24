(defpackage :dhs-db-connection-pooling-system
  (:use :cl :asdf))

(in-package :dhs-db-connection-pooling-system)

(defsystem dhs-db-connection-pooling
  :name "dhs-db-connection-pooling"
  :author "Elias Martenson <lokedhs@gmail.com>"
  :license "BSD"
  :description "Connection pool implementation for dhs-db"
  :depends-on (:dhs-db :bordeaux-threads)
  :components ((:module :dhs-db-connection-pooling
                        :pathname "connection-pooling"
                        :components ((:file "package")
                                     (:file "pooling" :depends-on ("package"))))))
