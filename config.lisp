(in-package :paymill)

(defvar *paymill-private-test-key* "")
(defvar *paymill-live-key* "")
(defvar *paymill-host* "api.paymill.com")

;;*PAYMILL-KEY* needs to be set to your private test key for testing.  If you
;;set it to your live key - you will be charged for the transactions.

(defvar *paymill-key* *paymill-private-test-key*)
