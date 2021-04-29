(asdf:defsystem :mita-mysql
  :serial t
  :pathname "src"

  :components
  ((:file "cffi")
   (:file "mysql"))

  :depends-on
  (:cffi))
