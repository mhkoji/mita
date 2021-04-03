(asdf:defsystem :mita-util-mysql
  :serial t
  :components
  ((:file "mysql"))
  :depends-on
  (:cl-dbi))
