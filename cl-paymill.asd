(in-package :asdf)

(defsystem "cl-paymill"

    :description "CL-PAYMILL is a common lisp interface to the Paymill
    payment service API.  See https://www.paymill.com"

    :version "0.0.1"
    :author "Peter Wood, email: pete_wood at runbox.com"
    :license "BSD, 2 clause."
    :depends-on (:drakma
                 :st-json 
                 :cl+ssl)
    :serial t
    :components ((:file "package") 
                 (:file "build")
                 (:file "paymill")))
