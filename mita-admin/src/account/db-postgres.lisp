(in-package :mita.postgres.db)

(defun parse-account (row)
  (mita.account.db:make-account
   :id (mita.id:parse (first row))
   :username (second row)
   :hashed-password (mita.account.db:make-hashed-password
                     :string (third row))))


(defmethod mita.account.db:account-insert
    ((db postgres)
     (account mita.account.db:account))
  (insert-into db "accounts" '("account_id" "username" "password_hashed")
               (list
                (list (mita.id:to-string
                       (mita.account.db:account-id account))
                      (mita.account.db:account-username
                       account)
                      (mita.account.db:hashed-password-string
                       (mita.account.db:account-hashed-password
                        account))))))

(defmethod mita.account.db:account-select ((db postgres)
                                           (username string))
  (single #'parse-account
          (select-from
           db "account_id, username, password_hashed" "accounts"
           `(:where (:= "username" (:p ,username))))))

(defmethod mita.account.db:account-select-by-id ((db postgres)
                                                 (id mita.id:id))
  (single #'parse-account
          (select-from
           db "account_id, username, password_hashed" "accounts"
           `(:where (:= "account_id" (:p ,(mita.id:to-string id)))))))
