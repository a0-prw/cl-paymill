;; Copyright (c) 2013, Peter Wood
;; All rights reserved.

;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions are
;; met:

;;     Redistributions of source code must retain the above copyright
;;     notice, this list of conditions and the following disclaimer.
    
;;     Redistributions in binary form must reproduce the above copyright
;;     notice, this list of conditions and the following disclaimer in
;;     the documentation and/or other materials provided with the
;;     distribution.

;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;; A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
;; HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;; SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
;; LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
;; DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
;; THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
;; OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

(in-package :paymill)

;;*PAYMILL-KEY* needs to be set to your private test key for testing.  If you
;;set it to your live key - YOU WILL BE CHARGED FOR THE TRANSACTIONS.

(defmacro initialize-paymill (key)
  "Until you call this macro, the access functions are not defined.
You should call it with a key designating your private test- or live-
key.  If you use a live key, you get charged by Paymill."
  `(progn
     (define-resource-access :clients (:new :update :retrieve :list :delete) ,key)
     (define-resource-access :payments (:new :retrieve :list :delete) ,key)
     (define-resource-access :transactions (:new :update :retrieve :list) ,key)
     (define-resource-access :refunds (:refund :retrieve :list) ,key)
     (define-resource-access :offers (:new :update :retrieve :list :delete) ,key)
     (define-resource-access :subscriptions (:new :retrieve :update :list :delete) ,key)
     (define-resource-access :webhooks (:new :update :retrieve :list :delete) ,key)
     t))


