(in-package :dhs-db-persist)

(declaim #.*compile-decl*)

(defun symbol-to-string (symbol)
  (cl-ppcre:regex-replace-all "-" (string-downcase (string symbol)) "_"))

(defun remove-keyword-from-list (arg-list keyword)
  (when arg-list
    (nconc (unless (eq (car arg-list) keyword)
             (list (car arg-list) (cadr arg-list)))
           (remove-keyword-from-list (cddr arg-list) keyword))))

(defun find-parameter (arg-list keyword)
  (when arg-list
    (if (eq (car arg-list) keyword)
        (cadr arg-list)
        (find-parameter (cddr arg-list) keyword))))

(defun find-slot-instance (class slot-name)
  (dolist (s (closer-mop:class-slots class))
    (when (eql (closer-mop:slot-definition-name s) slot-name)
      (return s))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PERSISTED-DB-ENTRY
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass persisted-db-entry ()
  ((modification-map :type hash-table
                     :reader db-entry-modification-map
                     :documentation "Map that keeps track of modifications made to the fields."))
  (:documentation "Common superclass for all persisted instances."))

(defmethod initialize-instance :before ((instance persisted-db-entry) &rest initargs)
  (declare (ignore initargs))
  (setf (slot-value instance 'modification-map) (make-hash-table :test 'eq)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PERSISTED-DB-ENTRY-CLASS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass persisted-db-entry-class (standard-class)
  ((db-table :type string
             :accessor persisted-db-entry-class-table
             :documentation "The name of the database table"))
  (:documentation "Metaclass for persisted instances"))

(defmethod closer-mop:validate-superclass ((class persisted-db-entry-class) (superclass standard-object))
  t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Modification tracking
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod (setf closer-mop:slot-value-using-class) :after (new-value (class persisted-db-entry-class) instance col)
  (when (and (slot-boundp instance 'modification-map) (persist-column col))
    (setf (gethash (closer-mop:slot-definition-name col) (db-entry-modification-map instance)) t)))

(defun clear-modifications (instance)
  (declare (type persisted-db-entry instance))
  (clrhash (db-entry-modification-map instance)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Slot definitions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass persisted-entry-class-slot-definition-mixin ()
  ((persist-column    :initarg :persist-column
                      :accessor persist-column
                      :documentation "Name of the column this field is persisted to,
NIL if the field is not persisted, or T to calculate the name of the column
based on the slot name.")
   (persist-id        :initarg :persist-id
                      :accessor persist-id
                      :documentation "If non-nil, designates this field as the primary key for this entry.
Currently, only one field can be the primary key.")
   (persist-mode      :initarg :persist-mode
                      :accessor persist-mode
                      :documentation "The type of persisting. Can be :READ if the field is initialised from the
database when read, :WRITE if it's written when saved or :BOTH. The default is :BOTH.")
   (persist-data-type :initarg :persist-data-type
                      :accessor persist-data-type
                      :documentation "The datatype of the column. Used as a hint to the backend implementation.")
   (persist-ref       :initarg :persist-ref
                      :accessor persist-ref
                      :documentation "Designates that this field is a foreign key reference to another persisted class.
The field specifies the type of the referenced object.")
   (persist-read      :initarg :persist-read
                      :accessor persist-read
                      :documentation "Conversion function to use when reading a value from the database.")
   (persist-store     :initarg :persist-store
                      :accessor persist-store
                      :documentation "Conversion function to use when writing a value to the database.")))

(defclass persisted-entry-direct-slot-definition (persisted-entry-class-slot-definition-mixin
                                                  closer-mop:standard-direct-slot-definition)
  ())

(defclass persisted-entry-effective-slot-definition (persisted-entry-class-slot-definition-mixin
                                                     closer-mop:standard-effective-slot-definition)
  ())

(defmethod closer-mop:direct-slot-definition-class ((class persisted-db-entry-class) &rest initargs)
  (declare (ignore initargs))
  (find-class 'persisted-entry-direct-slot-definition))

(defmethod closer-mop:effective-slot-definition-class ((class persisted-db-entry-class) &rest initargs)
  (declare (ignore initargs))
  (find-class 'persisted-entry-effective-slot-definition))

(defun ensure-slot-value (instance field-name &optional default-value)
  "Returns the value of slot FIELD-NAME in INSTANCE. If the slot is unbound, return DEFAULT-VALUE."
  (if (and (slot-exists-p instance field-name)
           (slot-boundp instance field-name))
      (slot-value instance field-name)
      default-value))

(defmethod closer-mop:compute-effective-slot-definition ((class persisted-db-entry-class) slot-name direct-slots)
  (let ((result (call-next-method)))
    (let ((p (ensure-slot-value (car direct-slots) 'persist-column)))
      ;; PERSIST-COLUMN
      (setf (persist-column result) (cond ((null p)          nil)
                                          ((eq p t)          (symbol-to-string slot-name))
                                          ((typep p 'string) p)
                                          (t (error "Illegal value for ~s parameter for slot ~s: ~s"
                                                    :persist-column slot-name p))))

      ;; PERSIST-ID
      (setf (persist-id result) (ensure-slot-value (car direct-slots) 'persist-id))

      ;; PERSIST-MODE
      (let ((m (ensure-slot-value (car direct-slots) 'persist-mode)))
        (setf (persist-mode result) (cond ((null m)
                                           :both)
                                          ((find m '(:read :write :both))
                                           m)
                                          (t
                                           (error "Illegal value for ~s parameter for slot ~s: ~s"
                                                  :persist-id slot-name m)))))

      ;; PERSIST-DATA-TYPE
      (setf (persist-data-type result) (ensure-slot-value (car direct-slots) 'persist-data-type))

      ;; PERSIST-REF
      (let ((type (ensure-slot-value (car direct-slots) 'persist-ref)))
        (unless (typep type '(or null symbol))
          (error "Illegal type for ~s parameter for slot ~s (must be SYMBOL or NIL): ~s"
                 :persist-ref slot-name (type-of type)))
        (setf (persist-ref result) type))

      ;; PERSIST-READ
      (setf (persist-read result) (ensure-slot-value (car direct-slots) 'persist-read #'identity))

      ;; PERSIST-STORE
      (setf (persist-store result) (ensure-slot-value (car direct-slots) 'persist-store #'identity)))
    
    result))

(macrolet ((init-reinit (name)
             `(defmethod ,name :around ((class persisted-db-entry-class)
                                        &rest args
                                        &key table-name direct-superclasses
                                        &allow-other-keys)
                (let ((root-class (find-class 'persisted-db-entry nil)))
                  (unless root-class
                    (error "Can't find a the class descriptor for ~s" 'persisted-db-entry))
                  (cond ((or (equal class root-class)
                             (some #'(lambda (s) (typep s 'persisted-db-entry-class)) direct-superclasses))
                         (call-next-method))
                        (t
                         (apply #'call-next-method
                                class
                                :direct-superclasses (append (list root-class)
                                                             direct-superclasses)
                                (remove-keyword-from-list args :direct-superclasses))))
                  (setf (persisted-db-entry-class-table class) (or (car table-name)
                                                                   (symbol-to-string (class-name class))))))))

  (init-reinit initialize-instance)
  (init-reinit reinitialize-instance))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Util methods
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-column-name (slot)
  (let ((name (persist-column slot)))
    (when name
      (list name))))

(defgeneric find-id-column (class)
  (:documentation "Finds the ID column for a given class"))

(defmethod find-id-column ((class-name symbol))
  (find-id-column (find-class class-name)))

(defmethod find-id-column ((class persisted-db-entry-class))
  (closer-mop:ensure-finalized class)
  (let ((cols (remove-if-not #'(lambda (v) (and (persist-id v) (persist-column v)))
                             (closer-mop:class-slots class))))
    (assert (= (length cols) 1))
    (car cols)))

(defun find-instance-id-value (instance)
  (let ((class (class-of instance)))
    (closer-mop:slot-value-using-class class instance (find-id-column class))))

(defun find-persisted-cols (class modes)
  "Return a list of all columns for which PERSIST-COLUMN returns non-nil
and the persist mode is one of the values found in MODES"
  (closer-mop:ensure-finalized class)
  (remove-if-not #'(lambda (v)
                     (and (slot-exists-p v 'persist-column)
                          (persist-column v)
                          (find (persist-mode v) modes)))
                 (closer-mop:class-slots class)))
