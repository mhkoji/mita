(defpackage :mita-mysql
  (:use :cl))
(in-package :mita-mysql)

(defvar *mysql-time-struct*
  '(:struct mita-mysql.cffi::mysql-time))

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


(defmacro time-year (time)
  `(cffi:foreign-slot-value
    ,time *mysql-time-struct* 'mita-mysql.cffi::year))

(defmacro time-month (time)
  `(cffi:foreign-slot-value
    ,time *mysql-time-struct* 'mita-mysql.cffi::month))

(defmacro time-day (time)
  `(cffi:foreign-slot-value
    ,time *mysql-time-struct* 'mita-mysql.cffi::day))

(defmacro time-hour (time)
  `(cffi:foreign-slot-value
    ,time *mysql-time-struct* 'mita-mysql.cffi::hour))

(defmacro time-minute (time)
  `(cffi:foreign-slot-value
    ,time *mysql-time-struct* 'mita-mysql.cffi::minute))

(defmacro time-second (time)
  `(cffi:foreign-slot-value
    ,time *mysql-time-struct* 'mita-mysql.cffi::second))


(defun string-to-octets (string)
  (babel:string-to-octets string :encoding :utf-8))

(defun octets-to-string (octets)
  (babel:octets-to-string octets :encoding :utf-8))


(defun datetime->lisp-value (time)
  (local-time:encode-timestamp 0
                               (time-second time)
                               (time-minute time)
                               (time-hour time)
                               (time-day time)
                               (time-month time)
                               (time-year time)))

(defun time->lisp-value (time)
  (make-instance 'local-time:timestamp
                 :nsec 0
                 :sec (+ (* (time-hour time) 3600)
                         (* (time-minute time) 60)
                         (time-second time))
                 :day 0))


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
    :documentation "holds a pointer to an instance of MYSQL"
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


(defun mysql-error (mysql)
  (error 'mysql-error
         :error (mita-mysql.cffi:mysql-error mysql)
         :errno (mita-mysql.cffi:mysql-errno mysql)))

(defun stmt-error (stmt)
  (error 'mysql-error
         :error (mita-mysql.cffi::mysql-stmt-error stmt)
         :errno (mita-mysql.cffi::mysql-stmt-errno stmt)))

(defun maybe-mysql-error (mysql ret)
  (if (= ret 0)
      ret
      (mysql-error mysql)))

(defun maybe-stmt-error (stmt ret)
  (if (= ret 0)
      ret
      (stmt-error stmt)))

(defun commit (conn)
  (let ((mysql (connection-mysql conn)))
    (maybe-mysql-error mysql (mita-mysql.cffi::mysql-commit mysql))))

(labels ((converter-for (sql-type)
           (ecase sql-type
             ((:longlong)
              (lambda (octets)
                (parse-integer (octets-to-string octets))))
             ((:string :var-string
               :datetime :time :date)
              #'octets-to-string)
             ((:blob)
              #'identity))))
  (defun query-octets->lisp-value (sql-type octets)
    (funcall (converter-for sql-type) octets)))

(defun fetch-query-result (mysql)
  ;; https://dev.mysql.com/doc/c-api/8.0/en/mysql-store-result.html
  ;; > it does not do any harm or cause any notable performance degradation if you call mysql_store_result() in all cases.
  (let ((res (mita-mysql.cffi::mysql-store-result mysql)))
    (if (cffi:null-pointer-p res)
        (when (/= (mita-mysql.cffi:mysql-errno mysql) 0)
          (mysql-error mysql))
        (unwind-protect
             (let* ((num-fields (mita-mysql.cffi::mysql-num-fields res))
                    (fields
                     (mita-mysql.cffi::mysql-fetch-fields res))
                    (field-types
                     (loop repeat num-fields
                           for i from 0
                           for field = (cffi:mem-aptr
                                        fields *mysql-field-struct* i)
                           collect (cffi:foreign-enum-keyword
                                    'mita-mysql.cffi::enum-field-types
                                    (field-type field)))))
               (loop for row = (mita-mysql.cffi::mysql-fetch-row res)

                     when (and (cffi:null-pointer-p row)
                               (/= (mita-mysql.cffi:mysql-errno mysql) 0))
                       do (mysql-error mysql)

                     while (not (cffi:null-pointer-p row))

                     collect
                     (let ((lens (mita-mysql.cffi::mysql-fetch-lengths res)))
                       (when (cffi:null-pointer-p lens)
                         (mysql-error mysql))
                       (loop repeat num-fields
                             for type in field-types
                             for i from 0
                             collect
                             (query-octets->lisp-value
                              type
                              (let ((nth-ptr
                                     (cffi:mem-aref row :pointer i))
                                    (len
                                     (cffi:mem-aref lens :unsigned-long i)))
                                (cffi:foreign-array-to-lisp
                                 nth-ptr
                                 (list :array :uint8 len)
                                 :element-type '(unsigned-byte 8))))))))
          (mita-mysql.cffi::mysql-free-result res)))))

(defun query (conn string)
  (let ((mysql (connection-mysql conn)))
    (maybe-mysql-error mysql (mita-mysql.cffi:mysql-query mysql string))
    (fetch-query-result mysql)))


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
      ((:date :datetime)
       (datetime->lisp-value (bind-buffer bind)))
      ((:time)
       (time->lisp-value (bind-buffer bind)))
      ((:string :blob)
       (let ((len (cffi:mem-ref (bind-length bind) :long)))
         (let ((octets (cffi:foreign-array-to-lisp
                        (bind-buffer bind)
                        (list :array :uint8 len)
                        :element-type '(unsigned-byte 8))))
           (if (eql sql-type :string)
               (babel:octets-to-string octets :encoding :utf-8)
               octets)))))))

(defun fetch-execute-result (stmt)
  (let ((res (mita-mysql.cffi::mysql-stmt-result-metadata stmt)))
    (when (cffi:null-pointer-p res)
      ;; https://dev.mysql.com/doc/c-api/8.0/en/mysql-stmt-fetch.html
      (stmt-error stmt))
    (unwind-protect
         ;; Fetch num fields to know the required count of binds.
         (let* ((num-fields (mita-mysql.cffi::mysql-num-fields res))
                (binds (cffi:foreign-alloc *mysql-bind-struct*
                                           :count num-fields)))
           (unwind-protect
                (progn
                  ;; Bind result
                  ;; Fetch fields to set field types to binds
                  (let ((fields (mita-mysql.cffi::mysql-fetch-fields res)))
                    (dotimes (i num-fields)
                      (let ((bind
                             (cffi:mem-aptr binds *mysql-bind-struct* i))
                            (field
                             (cffi:mem-aptr fields *mysql-field-struct* i)))
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
                          do (stmt-error stmt)
                        while (= ret 0)
                        collect
                        (loop for i from 0 below num-fields
                              collect
                              (bind->lisp-value
                               (cffi:mem-aptr binds *mysql-bind-struct* i)))))
             (dotimes (i num-fields)
               (bind-release (cffi:mem-aptr binds *mysql-bind-struct* i)))
             (cffi:foreign-free binds)))
      (mita-mysql.cffi::mysql-free-result res))))

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
                    (fetch-execute-result mysql-stmt))
               ;; binds are released after execution because execute seems to use the values in the bindings:
               ;; https://dev.mysql.com/doc/c-api/8.0/en/c-api-prepared-statement-data-structures.html
               ;; > When you call mysql_stmt_execute(), MySQL use the value stored in the variable
               ;; > in place of the corresponding parameter marker in the statement
               (dotimes (i num-params)
                 (bind-release (cffi:mem-aptr binds *mysql-bind-struct* i)))
               (cffi:foreign-free binds))))
      (mita-mysql.cffi::mysql-stmt-close mysql-stmt))))
