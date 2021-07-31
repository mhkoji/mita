(defpackage :mita.auth.admin
  (:use :cl)
  (:export :get-account-db
           :get-admin-db
           :account-root
           :create-account
           :delete-account
           :list-accounts
           :init))
(in-package :mita.auth.admin)

(defun account-id->db-name (id-string)
  (format nil "account_~A"
          (string-downcase
           (cl-ppcre:regex-replace-all "-" id-string "_"))))

(defun account-root (base account-id-string)
  (concatenate 'string base "/" (account-id->db-name account-id-string) "/"))

(defun get-account-db (db-manager account-id-string)
  (mita.auth.admin.db:get-db db-manager (account-id->db-name account-id-string)))

(defun get-admin-db (db-manager)
  (mita.auth.admin.db:get-db db-manager "admin"))

(defun create-account (db-manager
                       username password
                       content-base
                       thumbnail-base
                       &key (id (mita.id:gen)))
  (let ((account
         (mita.db:with-connection (conn (get-admin-db db-manager))
           (mita.db:with-tx (conn)
             (mita.auth.admin.account:create-account conn id username password)))))
    (let ((id-string (mita.id:to-string id)))
      (mita.auth.admin.db:create-database db-manager
                                          (account-id->db-name id-string))
      (ensure-directories-exist (account-root content-base id-string))
      (ensure-directories-exist (account-root thumbnail-base id-string)))
    account))

(defun delete-account (db-manager account-id content-base thumbnail-base)
  (let ((id-string (mita.id:to-string account-id)))
    (mita.auth.admin.db:drop-database db-manager
                                      (account-id->db-name id-string))

    (dolist (p (list (account-root content-base id-string)
                     (account-root thumbnail-base id-string)))
      (cl-fad:delete-directory-and-files p :if-does-not-exist :ignore)))

  (mita.db:with-connection (conn (get-admin-db db-manager))
    (mita.db:with-tx (conn)
      (mita.auth.admin.account:delete-account conn account-id))))

(defun list-accounts (db-manager)
  (mita.db:with-connection (conn (get-admin-db db-manager))
    (mita.auth.admin.account:load-accounts conn)))

;;;

(defun init (db-manager content-base thumbnail-base)
  (mita.auth.admin.db:create-admin-database db-manager "admin")
  (create-account db-manager "mita" "mita" content-base thumbnail-base
                  :id (mita.id:parse "7128DA4E-2B13-45CC-A0BF-78AEC1668E2C")))
