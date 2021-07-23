(in-package :mita.db.file)

(defmethod mita.auth.admin.account:find-account-with-password-checked
    ((conn connection)
     (username string)
     (password string))
  (let ((row (car
              (select-rows-if
               conn
               +account+
               (lambda (row)
                 (and (string= (second row) username)
                      (mita.util.password:hashed-password-matches-p
                       (third row)
                       password)))))))
    (when row
      (mita.auth.admin.account:construct-account
       :id (mita.id:parse (first row))
       :username (second row)))))

(defmethod mita.auth.admin.account:find-account ((conn connection)
                                                 (username string))
  (let ((row (car
              (select-rows-if
               conn
               +account+
               (lambda (row) (string= (second row) username))))))
    (when row
      (mita.auth.admin.account:construct-account
       :id (mita.id:parse (first row))
       :username (second row)))))

(defmethod mita.auth.admin.account:create-account ((conn connection)
                                                   (id mita.id:id)
                                                   (username string)
                                                   (password string))
  (alexandria:appendf (gethash +account+ (connection-table-hash conn))
                      (list 
                       (list (mita.id:to-string id)
                             username
                             (mita.util.password:hash password))))
  (mita.auth.admin.account:find-account conn username))
