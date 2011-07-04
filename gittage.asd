;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(defsystem :gittage
  :depends-on (:alexandria :iterate :split-sequence :cl-fad
               :pergamum :executor)
  :components
  ((:file "package")
   (:file "gittage" :depends-on ("package"))))
