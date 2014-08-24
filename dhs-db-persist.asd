(defpackage :dhs-db-persist-system
  (:use :cl :asdf))

(in-package :dhs-db-persist-system)

(defsystem dhs-db-persist
  :name "dhs-db-persist"
  :author "Elias Martenson <lokedhs@gmail.com>"
  :license "BSD"
  :description "Persistence framework for DHS-DB"
  :depends-on (:dhs-db
               :closer-mop
               :genhash)
  :components ((:module :dhs-db-persist
                        :pathname "persist"
                        :components ((:file "package")
                                     (:file "metaclasses" :depends-on ("package"))
                                     (:file "database-store" :depends-on ("metaclasses"))))))
