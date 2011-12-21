
(in-package :cl-user)

(defpackage #:org-parse-asd
  (:use :cl :asdf))

(in-package :org-parse-asd)

(defsystem org-parse
  :name "org-parse"
  :version "0.1"
  :depends-on (:hunchentoot
               :html-string)
  :serial t
  :components ((:file "org-parse")))
  