(defpackage :mita-mysql
  (:use :cl))
(in-package :mita-mysql)

(defclass connection ()
  ((mysql
    :initarg :mysql
    :reader connection-mysql)))

(defun connect (hostname username password database port flags)
  (let ((mysql (mita-mysql.cffi:mysql-real-connect
                (mita-mysql.cffi:mysql-init (cffi:null-pointer))
                (or hostname "127.0.0.1")
                (or username (cffi:null-pointer))
                (or password (cffi:null-pointer))
                (or database (cffi:null-pointer))
                (or port 0)
                (cffi:null-pointer)
                (or flags 196640))))
    (make-instance 'connection :mysql mysql)))

(defun disconnect (conn)
  (mita-mysql.cffi:mysql-close (connection-mysql conn)))

(defun query (conn string)
  (mita-mysql.cffi:mysql-query (connection-mysql conn) string))


(defstruct param sql-type value)

(defvar *mysql-bind-struct*
  '(:struct mita-mysql.cffi::mysql-bind))

(defvar *mysql-field-struct*
  '(:struct mita-mysql.cffi::mysql-field))

(defvar *ptr-mysql-bind-struct*
  '(:pointer (:struct mita-mysql.cffi::mysql-bind)))

(defvar *ptr-mysql-field-struct*
  '(:pointer (:struct mita-mysql.cffi::mysql-field)))

(defmacro field-type (field)
  `(cffi:foreign-slot-value
    ,field *mysql-field-struct* 'mita-mysql.cffi::type))

(defmacro bind-buffer (bind)
  `(cffi:foreign-slot-value
    ,bind *mysql-bind-struct* 'mita-mysql.cffi::buffer))

(defmacro bind-buffer-type (bind)
  `(cffi:foreign-slot-value
    ,bind *mysql-bind-struct* 'mita-mysql.cffi::buffer-type))

(defmacro bind-buffer-length (bind)
  `(cffi:foreign-slot-value
    ,bind *mysql-bind-struct* 'mita-mysql.cffi::buffer-length))

(defmacro bind-length (bind)
  `(cffi:foreign-slot-value
    ,bind *mysql-bind-struct* 'mita-mysql.cffi::length))

(defmacro bind-is-null (bind)
  `(cffi:foreign-slot-value
    ,bind *mysql-bind-struct* 'mita-mysql.cffi::is-null))

(defmacro bind-error (bind)
  `(cffi:foreign-slot-value
    ,bind *mysql-bind-struct* 'mita-mysql.cffi::error))

;; ref: https://dev.mysql.com/doc/c-api/8.0/en/mysql-bind-param.html
;; Maybe should split allocations for param and result.
(defun allocate-bind-fields (bind sql-type &optional value)
  (ecase sql-type
    ;; TODO: free memory
    ((:long :longlong)
     (setf (bind-buffer-type bind) (cffi:foreign-enum-value
                                    'mita-mysql.cffi::enum-field-types
                                    :long))
     (setf (bind-buffer bind)  (cffi:foreign-alloc :long)
           (bind-is-null bind) (cffi:null-pointer)
           (bind-length bind)  (cffi:null-pointer))
     (when value
       (setf (cffi:mem-ref (bind-buffer bind) :long) value)))
    ((:blob :string :var-string)
     ;; TODO: free memory
     (setf (bind-buffer-type bind) (cffi:foreign-enum-value
                                    'mita-mysql.cffi::enum-field-types
                                    :string))
     (setf (bind-buffer bind)        (cffi:foreign-alloc :char :count 1024)
           ;; https://dev.mysql.com/doc/c-api/8.0/en/mysql-stmt-fetch.html
           (bind-buffer-length bind) 1024
           (bind-is-null bind)       (cffi:null-pointer)
           (bind-length bind)        (cffi:foreign-alloc :long))
     (when value
       (let ((octets (babel:string-to-octets value :encoding :utf-8)))
         (let ((len (length octets)))
           (cffi:lisp-array-to-foreign
            octets (bind-buffer bind) (list :array :uint8 len))
           (setf (cffi:mem-ref (bind-length bind) :long) len)))))))

(defun make-binds (params)
  ;; TODO: free memory
  (let ((binds (cffi:foreign-alloc
                *mysql-bind-struct* :count (length params))))
    (loop for i from 0 for p in params do
      (let ((bind (cffi:mem-aptr binds *mysql-bind-struct* i)))
        (allocate-bind-fields bind (param-sql-type p) (param-value p))))
    binds))

(defmacro with-stmt-result-metadata ((var stmt) &body body)
  `(let ((,var (mita-mysql.cffi::mysql-stmt-result-metadata ,stmt)))
     (unwind-protect (progn ,@body)
       (mita-mysql.cffi::mysql-free-result ,var))))

(defun bind->lisp-value (bind)
  (ecase (cffi:foreign-enum-keyword 'mita-mysql.cffi::enum-field-types
                                    (bind-buffer-type bind))
    ((:long :longlong)
     (cffi:mem-ref (bind-buffer bind) :long))
    (:string
     (let ((len (cffi:mem-ref (bind-length bind) :long)))
       (let ((octets (cffi:foreign-array-to-lisp
                      (bind-buffer bind)
                      (list :array :uint8 len)
                      :element-type '(unsigned-byte 8))))
         (babel:octets-to-string octets :encoding :utf-8))))))

(defun fetch-result (stmt)
  (with-stmt-result-metadata (res stmt)
    ;; TODO: free memory
    ;; Fetch num fields to allocate binds
    (let* ((num-fields (mita-mysql.cffi::mysql-num-fields res))
           (binds (cffi:foreign-alloc
                   *mysql-bind-struct* :count num-fields)))
      ;; Fetch fields to set field types to binds
      (let ((fields (mita-mysql.cffi::mysql-fetch-fields res)))
        (dotimes (i num-fields)
          (let ((bind  (cffi:mem-aptr binds *mysql-bind-struct* i))
                (field (cffi:mem-aptr fields *mysql-field-struct* i)))
            (let ((sql-type (cffi:foreign-enum-keyword
                             'mita-mysql.cffi::enum-field-types
                             (field-type field))))
              (allocate-bind-fields bind sql-type)))))
      (print (cffi:mem-aref binds *mysql-bind-struct* 1))
      (print (mita-mysql.cffi::mysql-stmt-bind-result stmt binds))
      (loop for status = (print (mita-mysql.cffi::mysql-stmt-fetch stmt))
            while (= status 0)
            collect
            (loop for i from 0 below num-fields
                  collect
                  (bind->lisp-value
                   (cffi:mem-aptr binds *mysql-bind-struct* i)))))))

(defun execute (conn query params)
  (let ((mysql-stmt (mita-mysql.cffi::mysql-stmt-init
                     (connection-mysql conn))))
    (unwind-protect
         (progn
           ;; Prepare
           (let ((len (length (babel:string-to-octets
                               query :encoding :utf-8))))
             (mita-mysql.cffi::mysql-stmt-prepare mysql-stmt query len))
           ;; Bind
           (let ((binds (make-binds params)))
             (mita-mysql.cffi::mysql-stmt-bind-param mysql-stmt binds))
           ;; Execute
           (mita-mysql.cffi::mysql-stmt-execute mysql-stmt)
           ;; Fetch
           (fetch-result mysql-stmt))
      (mita-mysql.cffi::mysql-stmt-close mysql-stmt))))
