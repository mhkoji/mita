(in-package :mita.rdb.postgres)

(defun parse-account (row)
  (mita.admin.account.rdb:make-account
   :id (mita.id:parse (first row))
   :username (second row)
   :hashed-password (mita.util.password:make-hashed-password
                     :string (third row))))

(defmethod mita.admin.account.rdb:account-insert
    ((conn mita.rdb:connection)
     (account mita.admin.account.rdb:account))
  (mita.rdb.common:insert-into
   conn "accounts" '("account_id" "username" "password_hashed")
   (list
    (list (mita.id:to-string
           (mita.admin.account.rdb:account-id account))
          (mita.admin.account.rdb:account-username
           account)
          (mita.util.password:hashed-password-string
           (mita.admin.account.rdb:account-hashed-password
            account))))))

(defmethod mita.admin.account.rdb:account-select
    ((conn mita.rdb:connection)
     (username string))
  (mita.rdb.common::single
   #'parse-account
   (mita.rdb.common:select-from
    conn "account_id, username, password_hashed" "accounts"
    :where `(:= "username" (:p ,username)))))

(defmethod mita.admin.account.rdb:account-select-by-id
    ((conn mita.rdb:connection)
     (id mita.id:id))
  (mita.rdb.common::single
   #'parse-account
   (mita.rdb.common:select-from
    conn "account_id, username, password_hashed" "accounts"
    :where `(:= "account_id" (:p ,(mita.id:to-string id))))))

(defmethod mita.admin.account.rdb:account-select-all
    ((conn mita.rdb.postgres:connection))
  (mapcar #'parse-account
          (execute
           conn
           "SELECT account_id, username, password_hashed FROM accounts OFFSET $1 LIMIT $2"
           (list 0 50))))

(defmethod mita.admin.account.rdb:account-delete
    ((conn mita.rdb:connection)
     (id mita.id:id))
  (mita.rdb.common:delete-from
   conn "accounts"
   :where `(:= "account_id" (:p ,(mita.id:to-string id)))))
