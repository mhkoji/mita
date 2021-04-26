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

(defun set-param-to-bind (bind p)
  (with-accessors ((value param-value)
                   (sql-type param-sql-type)) p
    (setf (bind-buffer-type bind)
          (cffi:foreign-enum-value 'mita-mysql.cffi::enum-field-types
                                   sql-type))
    (ecase sql-type
      ;; TODO: free memory
      ((:long :longlong)
       (let ((buffer (cffi:foreign-alloc :long)))
         (setf (cffi:mem-ref buffer :long) value)
         (setf (bind-buffer bind) buffer))
       (setf (bind-length bind)  (cffi:null-pointer)
             (bind-is-null bind) (cffi:null-pointer)))
      ((:string :var-string)
       ;; TODO: free memory
       (let ((buffer (cffi:foreign-alloc :char :count 1024)))
         (cffi:lisp-string-to-foreign value buffer 1024)
         (setf (bind-buffer bind) buffer))
       (setf (bind-length bind)  (cffi:foreign-alloc :int)
             (bind-is-null bind) (cffi:null-pointer))))))

(defun make-binds (params)
  ;; TODO: free memory
  (let ((binds (cffi:foreign-alloc
                *mysql-bind-struct* :count (length params))))
    (loop for i from 0 for p in params do
      (let ((bind (cffi:mem-aptr binds *mysql-bind-struct* i)))
        (set-param-to-bind bind p)))
    binds))

(defmacro with-stmt-result-metadata ((var stmt) &body body)
  `(let ((,var (mita-mysql.cffi::mysql-stmt-result-metadata ,stmt)))
     (unwind-protect (progn ,@body)
       (mita-mysql.cffi::mysql-free-result ,var))))

(defun fetch-result (stmt)
  (with-stmt-result-metadata (res stmt)
    ;; TODO: free memory
    (let* ((num-fields (mita-mysql.cffi::mysql-num-fields res))
           (binds (cffi:foreign-alloc
                   *mysql-bind-struct* :count num-fields)))
      (let ((fields (mita-mysql.cffi::mysql-fetch-fields res)))
        (dotimes (i num-fields)
          (let ((bind  (cffi:mem-aptr binds *mysql-bind-struct* i))
                (field (cffi:mem-aptr fields *mysql-field-struct* i)))
            (print (getf (cffi:mem-ref field *mysql-field-struct*)
                         'mita-mysql.cffi::name))
            ;; TODO: free memory
            ;; TODO: should not restrict to only string support
            (setf (bind-buffer bind)
                  (cffi:foreign-alloc :char :count 1024)

                  (bind-buffer-type bind)
                  (field-type field))
            ;; TODO: free memory
            (setf (bind-length bind)  (cffi:foreign-alloc :int)
                  (bind-is-null bind) (cffi:foreign-alloc :char)
                  (bind-error bind)   (cffi:foreign-alloc :char)))))
      (mita-mysql.cffi::mysql-stmt-bind-result stmt binds)
      (loop
        for status = (print (mita-mysql.cffi::mysql-stmt-fetch stmt))
        while (= status 0)
        do (progn
             (dotimes (i num-fields)
               (let ((bind (cffi:mem-aptr binds *mysql-bind-struct* i)))
                 (let ((len (cffi:mem-ref (bind-length bind) :int)))
                   (print
                    (babel:octets-to-string
                     (cffi:foreign-array-to-lisp
                      (bind-buffer bind)
                      (list :array :uint8 len)
                      :element-type '(unsigned-byte 8))
                     :encoding :utf-8))))))))))

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
