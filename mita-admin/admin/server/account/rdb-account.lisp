(in-package :mita.admin.account.rdb)

(defmethod mita.admin.account:find-account-with-password-checked
    ((conn mita.db.rdb:connection)
     (username string)
     (password string))
  (when-let ((row (account-select conn username)))
    (when (mita.util.password:hashed-password-matches-p
           (account-hashed-password row)
           password)
      (mita.admin.account:construct-account
       :id (account-id row)
       :username (account-username row)))))

(defmethod mita.admin.account:find-account ((conn mita.db.rdb:connection)
                                            (username string))
  (when-let ((row (account-select conn username)))
    (mita.admin.account:construct-account
     :id (account-id row)
     :username (account-username row))))

(defmethod mita.admin.account:find-account-by-id
    ((conn mita.db.rdb:connection)
     (account-id mita.id:id))
  (when-let ((row (account-select-by-id conn account-id)))
    (mita.admin.account:construct-account
     :id (account-id row)
     :username (account-username row))))

(defmethod mita.admin.account:create-account ((conn mita.db.rdb:connection)
                                              (username string)
                                              (password string))
  (account-insert conn
                  (make-account
                   :id (mita.id:gen)
                   :username username
                   :hashed-password (mita.util.password:hash password)))
  (mita.admin.account:find-account conn username))

(defmethod mita.admin.account:delete-account ((conn mita.db.rdb:connection)
                                              (account-id mita.id:id))
  (account-delete conn account-id))

(defmethod mita.admin.account:load-accounts ((conn mita.db.rdb:connection))
  (mapcar (lambda (row)
            (mita.admin.account:construct-account
             :id (account-id row)
             :username (account-username row)))
          (account-select-all conn)))
