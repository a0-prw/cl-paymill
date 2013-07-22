(in-package :paymill)

(define-resource-access :clients (:new :update :retrieve :list :delete))
(define-resource-access :payments (:new :retrieve :list :delete))
(define-resource-access :transactions (:new :update :retrieve :list))
(define-resource-access :refunds (:refund :retrieve :list))
(define-resource-access :offers (:new :update :retrieve :list :delete))
(define-resource-access :subscriptions (:new :retrieve :update :list :delete))
