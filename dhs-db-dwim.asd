(defpackage :dhs-db-dwim-system
  (:use :cl :asdf))

(in-package :dhs-db-dwim-system)

(defsystem dhs-db-dwim
  :name "dhs-db-dwim"
  :author "Elias Martenson <lokedhs@gmail.com>"
  :license "BSD"
  :description "DHS-DB backend for hu.dwim.rdbms"
  :depends-on (:dhs-db :hu.dwim.rdbms)
  :components ((:module :dhs-db-dwim
                        :pathname "dwim"
                        :components ((:file "package")
                                     (:file "dhs-db-dwim" :depends-on ("package"))))))
