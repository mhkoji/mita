(in-package :mita.auth.admin.db)

(defmethod get-db ((dbm postgres-manager) db-name)
  (make-instance 'mita.db.vendor.postgres:postgres
                 :db-name db-name
                 :locator (get-locator dbm)))

(defmethod create-admin-database ((dbm postgres-manager) db-name)
  (postmodern:with-connection (mita.db.vendor.postgres::make-spec
                               db-name (get-locator dbm))
    (postmodern:execute-file
     (merge-pathnames (db-manager-db-dir dbm) "./admin-ddl.sql"))))

(defmethod create-database ((dbm postgres-manager) db-name)
  (let ((locator (get-locator dbm)))
    (postmodern:with-connection (mita.db.vendor.postgres::make-spec
                                 "admin" locator)
      (postmodern:query (format nil "CREATE DATABASE ~A" db-name)))
    (postmodern:with-connection (mita.db.vendor.postgres::make-spec
                                 db-name locator)
      (postmodern:execute-file
       (merge-pathnames (db-manager-db-dir dbm) "./mita-ddl.sql")))))

(defmethod drop-database ((dbm postgres-manager) db-name)
  (postmodern:with-connection (mita.db.vendor.postgres::make-spec
                               "admin" (get-locator dbm))
    (postmodern:query (format nil "DROP DATABASE IF EXISTS ~A" db-name))))
