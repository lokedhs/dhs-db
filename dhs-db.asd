(defpackage :dhs-db-system
  (:use :cl :asdf))

(in-package :dhs-db-system)

(defsystem dhs-db
  :name "dhs-db"
  :author "Elias Martenson <lokedhs@gmail.com>"
  :license "BSD"
  :description "Generic SQL interface"
  :long-description "Generic SQL inerface that acts as a frontend for the various SQL API's"
  :depends-on (:cl-ppcre)
  :components ((:module :dhs-db-api
                        :components ((:file "package")
                                     (:file "api" :depends-on ("package"))
                                     (:file "timestamp" :depends-on ("package"))
                                     (:file "prepared-statement" :depends-on ("api"))
                                     (:file "connection" :depends-on ("api"))))))
