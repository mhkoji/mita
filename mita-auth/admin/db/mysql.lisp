(in-package :mita.auth.admin.db)

(defmethod get-db ((dbm mysql-manager) db-name)
  (make-instance 'mita.db.vendor.mysql:mysql
                 :db-name db-name
                 :locator (get-locator dbm)))

(defmethod create-admin-database ((dbm mysql-manager) db-name)
  (mita.db.vendor.mysql:create-database
   (get-locator dbm)
   db-name
   (merge-pathnames (db-manager-db-dir dbm) "./admin-ddl.sql")))

(defmethod create-database ((dbm mysql-manager) db-name)
  (mita.db.vendor.mysql:create-database
   (get-locator dbm)
   db-name
   (merge-pathnames (db-manager-db-dir dbm) "./mita-ddl.sql")))

(defmethod drop-database ((dbm mysql-manager) db-name)
  (mita.db.vendor.mysql:drop-database (get-locator dbm) db-name))
