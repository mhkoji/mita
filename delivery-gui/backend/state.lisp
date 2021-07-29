;; Commonly used states
(defpackage :mita.gui.state
  (:use :cl)
  (:export :loading
           :make-loading))
(in-package :mita.gui.state)

(defstruct loading)
