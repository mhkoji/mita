(asdf:defsystem :mita-mysql
  :serial t
  :pathname "src"

  :components
  ((:file "cffi"))

  :depends-on
  (:cffi))
