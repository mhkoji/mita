(in-package :mita.db.file)

(defvar +account+ "account")

(defmethod mita.account.db:account-insert ((db file-db)
                                           (account mita.account.db:account))
  (push (list (mita.id:to-string
               (mita.account.db:account-id account))
              (mita.account.db:account-username account)
              (mita.util.password:hashed-password-string
               (mita.account.db:account-hashed-password account)))
        (gethash +account+ (table-hash db))))

(labels ((parse-account (row)
           (mita.account.db:make-account
            :id (mita.id:parse (first row))
            :username (second row)
            :hashed-password (mita.util.password:make-hashed-password
                              :string (third row)))))
  (defmethod mita.account.db:account-select ((db file-db)
                                             (username string))
    (car (mapcar
          #'parse-account
          (select-rows-if db +account+
                          (lambda (row)
                            (string= (second row) username))))))

  (defmethod mita.account.db:account-select-by-id ((db file-db)
                                                   (id mita.id:id))
    (car (mapcar
          #'parse-account
          (select-rows-if db +account+
                          (lambda (row)
                            (mita.id:id= (mita.id:parse (first row))
                                         id)))))))

;;;;

(defun create-account-database (connector account)
  (let ((hash (make-hash-table :test #'equal)))
    (dolist (name (list +image+
                        +album+
                        +album-thumbnail-image+
                        +album-image+))
      (setf (gethash name hash) nil))
    (write-table-hash (format nil "~A/~A/"
                              (connector-dir connector)
                              (account-table-name account))
                      hash)))

(defun create-account (connector username password)
  (let ((account
         (with-admin-db (db connector)
           (mita.account:create-account db username password))))
    (create-account-database connector account)
    account))

(defun init (connector)
  (let ((hash (make-hash-table :test #'equal)))
    (dolist (name (list +account+))
      (setf (gethash name hash) nil))
    (write-table-hash (connector-dir connector) hash))
  (create-account connector "mita" "mita"))
