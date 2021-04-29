(defpackage :mita-mysql
  (:use :cl))
(in-package :mita-mysql)

(defun string-to-octets (string)
  (babel:string-to-octets string :encoding :utf-8))

(defun octets-to-string (octets)
  (babel:octets-to-string octets :encoding :utf-8))

(define-condition mysql-error (error)
  ((error :initarg :error :reader mysql-error-error)
   (errno :initarg :errno :reader mysql-error-errno))
  (:report (lambda (condition stream)
             (format stream "MySQL error: \"~A\" (errno = ~D)."
                     (mysql-error-error condition)
                     (mysql-error-errno condition)))))

(defclass connection ()
  ((mysql
    :initarg :mysql
    :reader connection-mysql)))

(defstruct param sql-type value)

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
    (when (cffi:null-pointer-p mysql)
      (error "Failed to connect"))
    (make-instance 'connection :mysql mysql)))

(defun disconnect (conn)
  (mita-mysql.cffi:mysql-close (connection-mysql conn)))

(defun query (conn string)
  (mita-mysql.cffi:mysql-query (connection-mysql conn) string))


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


(defun bind-allocate-long (bind sql-type)
  (setf (bind-buffer-type bind) (cffi:foreign-enum-value
                                 'mita-mysql.cffi::enum-field-types
                                 sql-type)

        (bind-buffer bind) (cffi:foreign-alloc :long)
        
        (bind-is-null bind) (cffi:null-pointer)

        (bind-length bind) (cffi:null-pointer)))

;; https://dev.mysql.com/doc/refman/5.6/ja/c-api-prepared-statement-date-handling.html
(defun bind-allocate-date (bind sql-type)
  (setf (bind-buffer-type bind) (cffi:foreign-enum-value
                                 'mita-mysql.cffi::enum-field-types
                                 sql-type)

        (bind-buffer bind) (cffi:foreign-alloc
                            '(:struct mita-mysql.cffi::mysql-time))

        (bind-is-null bind) (cffi:null-pointer)

        (bind-length bind) (cffi:null-pointer)))

(defun bind-allocate-string (bind sql-type &optional (length 1024))
  (setf (bind-buffer-type bind) (cffi:foreign-enum-value
                                 'mita-mysql.cffi::enum-field-types sql-type)

        (bind-buffer bind) (cffi:foreign-alloc :char :count length)

        ;; https://dev.mysql.com/doc/c-api/8.0/en/mysql-stmt-fetch.html
        (bind-buffer-length bind) length

        (bind-is-null bind) (cffi:null-pointer)

        (bind-length bind) (cffi:foreign-alloc :long)))

(defun bind-release (bind)
  (ignore-errors
   (cffi:foreign-free (bind-buffer bind)))

  (when (not (cffi:null-pointer-p (bind-length bind)))
    (ignore-errors
     (cffi:foreign-free (bind-length bind)))))


;; ref: https://dev.mysql.com/doc/c-api/8.0/en/mysql-bind-param.html
(defun setup-bind-for-param (bind param)
  (with-accessors ((sql-type param-sql-type)
                   (value param-value)) param
    (ecase sql-type
      ((:long)
       (bind-allocate-long bind :long)
       (setf (cffi:mem-ref (bind-buffer bind) :long) value))
      ((:string)
       (bind-allocate-string bind :string)
       (let* ((octets (string-to-octets value))
              (len (length octets)))
         (cffi:lisp-array-to-foreign octets
                                     (bind-buffer bind)
                                     (list :array :uint8 len))
         (setf (cffi:mem-ref (bind-length bind) :long) len))))))

(defun setup-bind-for-result (bind sql-type)
  (ecase sql-type
    ((:long :longlong)
     (bind-allocate-long bind sql-type))
    ((:date :time :datetime)
     (bind-allocate-date bind sql-type))
    ((:blob :string :var-string)
     (bind-allocate-string bind sql-type))))

(defun bind->lisp-value (bind)
  (let ((sql-type (cffi:foreign-enum-keyword
                   'mita-mysql.cffi::enum-field-types
                   (bind-buffer-type bind))))
    (ecase sql-type
      ((:long :longlong)
       (cffi:mem-ref (bind-buffer bind) :long))
      ((:date :time :datetime)
       (cffi:mem-ref (bind-buffer bind)
                     '(:struct mita-mysql.cffi::mysql-time)))
      ((:string :blob)
       (let ((len (cffi:mem-ref (bind-length bind) :long)))
         (let ((octets (cffi:foreign-array-to-lisp
                        (bind-buffer bind)
                        (list :array :uint8 len)
                        :element-type '(unsigned-byte 8))))
           (if (eql sql-type :string)
               (babel:octets-to-string octets :encoding :utf-8)
               octets)))))))

(defmacro with-stmt-result-metadata ((var stmt) &body body)
  `(let ((,var (mita-mysql.cffi::mysql-stmt-result-metadata ,stmt)))
     (unwind-protect (progn ,@body)
       (mita-mysql.cffi::mysql-free-result ,var))))

(defun stmt-error (stmt)
  (error 'mysql-error
         :error (mita-mysql.cffi::mysql-stmt-error stmt)
         :errno (mita-mysql.cffi::mysql-stmt-errno stmt)))

(defun maybe-stmt-error (stmt ret)
  (if (= ret 0)
      ret
      (stmt-error stmt)))

(defun fetch-result (stmt)
  (with-stmt-result-metadata (res stmt)
    ;; Fetch num fields to know the required count of binds.
    (let* ((num-fields (mita-mysql.cffi::mysql-num-fields res))
           (binds (cffi:foreign-alloc
                   *mysql-bind-struct* :count num-fields)))
      (unwind-protect
           (progn
             ;; Fetch fields to set field types to binds
             (let ((fields (mita-mysql.cffi::mysql-fetch-fields res)))
               (dotimes (i num-fields)
                 (let ((bind (cffi:mem-aptr binds *mysql-bind-struct* i))
                       (field (cffi:mem-aptr fields *mysql-field-struct* i)))
                   (let ((sql-type (cffi:foreign-enum-keyword
                                    'mita-mysql.cffi::enum-field-types
                                    (field-type field))))
                     (setup-bind-for-result bind sql-type)))))
             (maybe-stmt-error
              stmt
              (mita-mysql.cffi::mysql-stmt-bind-result stmt binds))

             ;; Fetch rows
             (loop for ret = (mita-mysql.cffi::mysql-stmt-fetch stmt)
                   when (= ret 1)
                     do (mysql-stmt-error stmt)
                   while (= ret 0)
                   collect
                   (loop for i from 0 below num-fields
                         collect
                         (bind->lisp-value
                          (cffi:mem-aptr binds *mysql-bind-struct* i)))))
        (dotimes (i num-fields)
          (bind-release (cffi:mem-aptr binds *mysql-bind-struct* i)))
        (cffi:foreign-free binds)))))

(defun execute (conn query params)
  (let ((mysql-stmt (mita-mysql.cffi::mysql-stmt-init
                     (connection-mysql conn))))
    (when (cffi:null-pointer-p mysql-stmt)
      (error "Failed to initialize statement"))
    (unwind-protect
         (progn
           ;; Prepare
           (let ((len (length (string-to-octets query))))
             (maybe-stmt-error
              mysql-stmt
              (mita-mysql.cffi::mysql-stmt-prepare mysql-stmt query len)))
           (let* ((num-params (length params))
                  (binds (cffi:foreign-alloc
                          *mysql-bind-struct* :count num-params)))
             (unwind-protect
                  (progn
                    ;; Bind
                    (loop for i from 0
                          for param in params
                          for bind = (cffi:mem-aptr
                                      binds *mysql-bind-struct* i)
                          do (setup-bind-for-param bind param))
                    (maybe-stmt-error
                     mysql-stmt                     
                     (mita-mysql.cffi::mysql-stmt-bind-param
                      mysql-stmt binds))

                    ;; Execute
                    (maybe-stmt-error
                     mysql-stmt
                     (mita-mysql.cffi::mysql-stmt-execute mysql-stmt))

                    ;; Fetch
                    (fetch-result mysql-stmt))
               ;; binds are released after execution because execute seems to use the values in the bindings:
               ;; https://dev.mysql.com/doc/c-api/8.0/en/c-api-prepared-statement-data-structures.html
               ;; > When you call mysql_stmt_execute(), MySQL use the value stored in the variable
               ;; > in place of the corresponding parameter marker in the statement
               (dotimes (i num-params)
                 (bind-release (cffi:mem-aptr binds *mysql-bind-struct* i)))
               (cffi:foreign-free binds))))
      (mita-mysql.cffi::mysql-stmt-close mysql-stmt))))
