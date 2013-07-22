(in-package :paymill)

;;*PAYMILL-KEY* needs to be set to your private test key for testing.  If you
;;set it to your live key - YOU WILL BE CHARGED FOR THE TRANSACTIONS.

(defun use-key (key)
  "Call this function with your private test- or live-key to define
access functions to the Paymill API.  If you use a live-key, you will
be charged for any transactions which are made."
  (setf *paymill-key* key)
  (define-resource-access :clients (:new :update :retrieve :list :delete))
  (define-resource-access :payments (:new :retrieve :list :delete))
  (define-resource-access :transactions (:new :update :retrieve :list))
  (define-resource-access :refunds (:refund :retrieve :list))
  (define-resource-access :offers (:new :update :retrieve :list :delete))
  (define-resource-access :subscriptions (:new :retrieve :update :list :delete))
  t)

