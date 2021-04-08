(in-package :mita.db.relational)

(defun parse-account (row)
  (mita.admin.account.db:make-account
   :id (mita.id:parse (first row))
   :username (second row)
   :hashed-password (mita.util.password:make-hashed-password
                     :string (third row))))

(defmethod mita.admin.account.db:account-insert
    ((conn connection)
     (account mita.admin.account.db:account))
  (insert-into conn "accounts" '("account_id" "username" "password_hashed")
               (list
                (list (mita.id:to-string
                       (mita.admin.account.db:account-id account))
                      (mita.admin.account.db:account-username
                       account)
                      (mita.util.password:hashed-password-string
                       (mita.admin.account.db:account-hashed-password
                        account))))))

(defmethod mita.admin.account.db:account-select ((conn connection)
                                                 (username string))
  (single #'parse-account
          (select-from
           conn "account_id, username, password_hashed" "accounts"
           :where `(:= "username" (:p ,username)))))

(defmethod mita.admin.account.db:account-select-by-id ((conn connection)
                                                       (id mita.id:id))
  (single #'parse-account
          (select-from
           conn "account_id, username, password_hashed" "accounts"
           :where `(:= "account_id" (:p ,(mita.id:to-string id))))))

(defmethod mita.admin.account.db:account-select-all
    ((conn mita.db.postgres:connection))
  (mapcar #'parse-account
          (mita.db.postgres::execute
           conn
           "SELECT account_id, username, password_hashed FROM accounts OFFSET $1 LIMIT $2"
           (list 0 50))))

(defmethod mita.admin.account.db:account-delete ((conn connection)
                                                 (id mita.id:id))
  (delete-from conn "accounts"
   :where `(:= "account_id" (:p ,(mita.id:to-string id)))))
