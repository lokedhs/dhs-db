(in-package :dhs-db)

(declaim #.*compile-decl*)

(defclass default-prepared-statement (dhs-db:prepared-statement)
  ((connection   :type connection
                 :initarg :connection
                 :initform (error "~s parameter required" :connection)
                 :reader default-prepared-statement-connection)
   (parts        :type list
                 :initarg :parts
                 :initform (error "~s parameter required" :parts)
                 :reader default-prepared-statement-parts))
  (:documentation "Default implementation of prepared statements."))

(defmethod print-object ((obj default-prepared-statement) out)
  (with-slots (parts) obj
    (print-unreadable-object (obj out :type t :identity t)
      (format out "~s" parts))))

(defun default-prepare-statement (connection statement-specifier)
  (make-instance 'default-prepared-statement
                 :connection connection
                 :parts statement-specifier))

(defun escape-string (s out)
  (declare (type string s)
           (type stream out))
  (loop
     for ch across s
     do (case ch
          ((#\')                (princ "''" out))
          ((#\Newline #\Return) (error "Illegal character in argument: ~s" ch))
          (t                    (princ ch out)))))

(defun apply-args-to-statement (parts args)
  (declare (type list parts args))
  (with-output-to-string (out)
    (loop
       with len = (length parts)
       for i from 0
       for part in parts
       do (etypecase part
            (string (princ part out))
            (integer (let ((arg (nth part args)))
                                  (etypecase arg
                                    (string  (princ "'" out) (escape-string arg out) (princ "'" out))
                                    (integer (format out "~d" arg))
                                    (float   (format out "~10$" arg))
                                    (number  (format out "~a" arg))
                                    (database-timestamp (format out "'~a'" (format-date arg)))
                                    (null    (format out "null"))))))
       if (< i (1- len)) do (princ #\Space out))))

(defmacro defprepared (method backend-impl include-result-types)
  `(defmethod ,method ((statement default-prepared-statement) args ,@(when include-result-types
                                                                           '(&key result-types)))
     (,backend-impl (default-prepared-statement-connection statement)
                    (apply-args-to-statement (default-prepared-statement-parts statement) args)
                    ,@(when include-result-types
                            '(:result-types result-types)))))

(defprepared dhs-db:execute-prepared-select dhs-db:select t)
(defprepared dhs-db:execute-prepared dhs-db:execute nil)

(defmethod dhs-db:close-prepared-statement ((statement default-prepared-statement))
  ;; No need to do anything
  nil)
