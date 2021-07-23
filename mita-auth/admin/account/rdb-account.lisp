(in-package :mita.auth.admin.account.rdb)

(defmethod mita.auth.admin.account:find-account-with-password-checked
    ((conn mita.db.rdb:connection)
     (username string)
     (password string))
  (when-let ((row (account-select conn username)))
    (when (mita.util.password:hashed-password-matches-p
           (account-hashed-password row)
           password)
      (mita.auth.admin.account:construct-account
       :id (account-id row)
       :username (account-username row)))))

(defmethod mita.auth.admin.account:find-account ((conn mita.db.rdb:connection)
                                                 (username string))
  (when-let ((row (account-select conn username)))
    (mita.auth.admin.account:construct-account
     :id (account-id row)
     :username (account-username row))))

(defmethod mita.auth.admin.account:find-account-by-id
    ((conn mita.db.rdb:connection)
     (account-id mita.id:id))
  (when-let ((row (account-select-by-id conn account-id)))
    (mita.auth.admin.account:construct-account
     :id (account-id row)
     :username (account-username row))))

(defmethod mita.auth.admin.account:create-account ((conn mita.db.rdb:connection)
                                              (id mita.id:id)
                                                   (username string)
                                                   (password string))
  (account-insert conn
                  (make-account
                   :id id
                   :username username
                   :hashed-password (mita.util.password:hash password)))
  (mita.auth.admin.account:find-account conn username))

(defmethod mita.auth.admin.account:delete-account ((conn mita.db.rdb:connection)
                                              (account-id mita.id:id))
  (account-delete conn account-id))

(defmethod mita.auth.admin.account:load-accounts ((conn mita.db.rdb:connection))
  (mapcar (lambda (row)
            (mita.auth.admin.account:construct-account
             :id (account-id row)
             :username (account-username row)))
          (account-select-all conn)))
