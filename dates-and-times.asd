;;;; dates-and-times.asd

(asdf:defsystem #:dates-and-times
  :description "Utilities for working with dates and times"
  :author "Peyton Farrar <peyton@peytonfarrar.com>"
  :license "MIT"
  :serial t
  :depends-on (#:peyton-utils)
  :components ((:file "package")
               (:file "dates-and-times")))
