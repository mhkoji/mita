(in-package :mita)

(defmacro with-gateway ((gw connector) &body body)
  (let ((g (gensym)))
    `(mita.db.postgres:with-transaction (,g ,connector)
       (let ((,gw (make-instance 'gateway :db ,g)))
         ,@body))))
