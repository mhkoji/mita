(in-package :mita.db.vendor.postgres)

(defmethod mita.auth.admin.db:get-db
    ((dbm mita.auth.admin.db:postgres-manager)
     db-name)
  (make-instance 'postgres
                 :db-name db-name
                 :locator (mita.auth.admin.db:get-locator dbm)))

(defmethod mita.auth.admin.db:create-admin-database
    ((dbm mita.auth.admin.db:postgres-manager)
     db-name)
  (postmodern:with-connection (make-spec db-name
                                         (mita.auth.admin.db:get-locator dbm))
    (postmodern:execute-file
     (merge-pathnames (mita.auth.admin.db:db-manager-db-dir dbm)
                      "./admin-ddl.sql"))))

(defmethod mita.auth.admin.db:create-database
    ((dbm mita.auth.admin.db:postgres-manager)
     db-name)
  (let ((locator (mita.auth.admin.db:get-locator dbm)))
    (postmodern:with-connection (make-spec "admin" locator)
      (postmodern:query (format nil "CREATE DATABASE ~A" db-name)))
    (postmodern:with-connection (make-spec db-name locator)
      (postmodern:execute-file
       (merge-pathnames (mita.auth.admin.db:db-manager-db-dir dbm)
                        "./mita-ddl.sql")))))

(defmethod mita.auth.admin.db:drop-database
    ((dbm mita.auth.admin.db:postgres-manager)
     db-name)
  (postmodern:with-connection (make-spec "admin"
                                         (mita.auth.admin.db:get-locator dbm))
    (postmodern:query (format nil "DROP DATABASE IF EXISTS ~A" db-name))))

(defmethod mita.auth.admin.account.rdb:account-select-all
    ((conn mita.db.vendor.postgres:connection))
  (mapcar
   #'mita.auth.admin.db.rdb.common:parse-account
   (execute
    conn
    "SELECT account_id, username, password_hashed FROM accounts OFFSET $1 LIMIT $2"
    (list 0 50))))
