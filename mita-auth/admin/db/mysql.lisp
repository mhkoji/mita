(in-package :mita.db.vendor.mysql)

(defmethod mita.auth.admin.db:get-db
    ((dbm mita.auth.admin.db:mysql-manager)
     db-name)
  (make-instance 'mysql
                 :db-name db-name
                 :locator (mita.auth.admin.db::get-locator dbm)))

(defmethod mita.auth.admin.db:create-admin-database
    ((dbm mita.auth.admin.db:mysql-manager)
     db-name)
  (let ((locator (mita.auth.admin.db:get-locator dbm))
        (ddl-file (merge-pathnames (mita.auth.admin.db:db-manager-db-dir dbm)
                                   "./admin-ddl.sql")))
    (create-database locator db-name ddl-file)))

(defmethod mita.auth.admin.db:create-database
    ((dbm mita.auth.admin.db:mysql-manager)
     db-name)
  (let ((locator (mita.auth.admin.db:get-locator dbm))
        (ddl-file (merge-pathnames (mita.auth.admin.db:db-manager-db-dir dbm)
                                   "./mita-ddl.sql")))
    (create-database locator db-name ddl-file)))

(defmethod mita.auth.admin.db:drop-database
    ((dbm mita.auth.admin.db:mysql-manager)
     db-name)
  (drop-database (mita.auth.admin.db:get-locator dbm) db-name))


(defmethod mita.auth.admin.account.rdb:account-select-all
    ((conn mita.db.vendor.mysql:connection))
  (mapcar
   (lambda (plist)
     (let ((row (mapcar #'cdr (alexandria:plist-alist plist))))
       (mita.auth.admin.db.rdb.common:parse-account row)))
   (execute
    conn
    "SELECT account_id, username, password_hashed FROM accounts LIMIT ?,?"
    (list 0 50))))
