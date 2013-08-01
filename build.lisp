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

;;See https://www.paymill.com/ to get an account and test keys, etc.  Read
;;their documentation to learn how to integrate their service into
;;your website

(defvar *paymill-key* "")

(defvar *paymill-host* "api.paymill.com")

;;Do not use this variable for anything. It is bound inside a macro.
(defvar *pm-reply*)

(defvar *cookie-jar* (make-instance 'drakma:cookie-jar))

(defun clear-cookie-jar ()
  (drakma:delete-old-cookies *cookie-jar*))

(defun resource-from-keyword (keyword)
  (string-downcase (symbol-name keyword)))

(defun normalize-slot (sl)
  (cond  ((keywordp sl)
          (resource-from-keyword sl))
         ((stringp sl) sl)
         (t (error "Could not normalize slot: ~S" sl))))

(defun slots (dotted &rest pairs)
  (labels ((rec (rst &optional (ret nil))
             (if (null rst) ret
                 (let ((sl (normalize-slot (car rst)))
                       (val (cadr rst)))
                   (rec (cddr rst) (cons (if dotted 
                                      (cons sl val)
                                      (list sl val))
                                  ret))))))
    (rec pairs)))

(defun x-www-form-encode-pairs (&rest pairs)
  (setf pairs (apply #'slots `(nil ,@pairs)))
  (let ((string "")
        (more (< 1 (length pairs))))
    (loop for (var val) in pairs
       do (setf string (concatenate 'string string var "=" val
                                    (if more "&" ""))))
    (string-right-trim '(#\&) string)))

;;WARNING: It is unlikely that it will always be possible to
;;report the error correctly or sensibly with the report function.  I
;;don't know enough about how Paymill structures their error replies
;;to be correct.  However, presumably you are going to be
;;handling the errors according to condition class, so the reporting
;;function shouldn't matter in production code.

;;In addition, there are MANY return codes internal to Paymill json
;;objects which are returned from a call.  Your application should
;;check these. See Paymill's documentation for details.

(define-condition paymill-error (simple-error) 
  ((reply :initarg :reply :reader reply)
   (status :initarg :status :reader status))
  (:report (lambda (c s) 
             (let* ((r (reply c))
                    (err (when (typep r 'st-json:jso)
                           (st-json:getjso "error" r)))
                    (msgs (cond ((and err (typep err 'st-json:jso))
                                 (st-json:getjso "messages" err))
                                ((and err (stringp err))
                                 (st-json:jso "err" err))
                                (t nil))))
               (format s "Request not OK (~D): ~{~A~%~}"
                       (status c) (if (null msgs)
                                      ()
                                      (let ((collect ()))
                                        (st-json:mapjso 
                                         #'(lambda (k v)
                                             (declare (ignore k))
                                             (setf collect (push v collect)))
                                         msgs)
                                        collect) ))))))

(define-condition paymill-general-error (paymill-error) ())
(define-condition paymill-bad-request (paymill-error) ())
(define-condition paymill-unauthorized (paymill-error) ())
(define-condition paymill-transaction-error (paymill-error) ())
(define-condition paymill-not-found (paymill-error) ())
(define-condition paymill-precondition-failed (paymill-error) ())
(define-condition paymill-server-error (paymill-error) ())

(defun signal-paymill-error (status reply)
  (when (<= 500 status 505)
    (error 'paymill-server-error :reply reply :status status))
  (case status
    (400 (error 'paymill-bad-request :reply reply :status status))
    (401 (error 'paymill-unauthorized :reply reply :status status))
    (403 (error 'paymill-transaction-error :reply reply :status status))
    (404 (error 'paymill-not-found :reply reply :status status))
    (412 (error 'paymill-precondition-failed :reply reply :status status))
    (t (error 'paymill-general-error :reply reply :status status))))

;; (define-condition api-internal-error (simple-error) 
;;   ((caller :initarg :caller :reader caller)
;;    (message :initarg :message :reader message))
;;   (:report (lambda (c s)
;;              (format s "~S signalled an error: ~A"
;;                      (caller c) (message c)))))

(defmacro with-pm-request (request-form &body body)
  "Does error checking of the reply from Paymill resulting from
  REQUEST-FORM.  If there are no errors, it binds (per-thread,
  locally) the variable *PM-REPLY* to the data associated with
  Paymill's reply for use in BODY, otherwise it signals 'an
  appropriate error'."
  (let ((reply (gensym "RP"))
        (status (gensym "ST"))
        (jso (gensym "JSO"))
        (tmp (gensym "TMP")))
     `(multiple-value-bind (,reply ,status)
          ,request-form
        (setf (flexi-streams:flexi-stream-external-format ,reply) :utf-8)
        (if (= ,status 200)
            (let* ((,jso (st-json:read-json ,reply))
                   (,tmp (st-json:getjso "data" ,jso))
                   (*pm-reply* (if ,tmp ,tmp ,jso)))
              ,@body)
            (signal-paymill-error ,status ,reply)))))


(defun pm-uri (resource &optional (target nil targetp))
  "RESOURCE is a keyword specifying a paymill api interface.  TARGET
is an optional generalized boolean (must be a string if it is given)
which is used to determine if path should end in /TARGET."
  (make-instance 'puri:uri 
                 :scheme :https
                 :host *paymill-host*
                 :path (if targetp 
                           (concatenate 'string "/v2/"
                                        (resource-from-keyword resource)
                                        "/" target)
                           (concatenate 'string "/v2/"
                                        (resource-from-keyword resource)))))

(defun pm-quri (resource query)
  "Like PM-URI, but for designating QUERY (string) parameters when
RESOURCE is the target."
  (make-instance 'puri:uri 
                 :scheme :https
                 :host *paymill-host*
                 :path (concatenate 'string "/v2/"
                                    (resource-from-keyword resource))
                 :query query))

(defmacro define-resource-access (resource instr-list)
  "This macro defines a uniform way of defining access to Paymill API
resources.  See README  for usage."
  (let ((access-name (intern (symbol-name resource) :paymill)))
    `(progn (defgeneric ,access-name (instruction &key &allow-other-keys))
            ,@(remove-if 
               #'null 
               (loop for instr in instr-list
                  collect (cond ((eql instr :new)
                                 `(defmethod ,access-name
                                      ((inst (eql :new)) &key (data nil))
                                    (with-pm-request
                                        (drakma:http-request (pm-uri ,resource)
                                                             :basic-authorization (list *paymill-key*) :method :post
                                                             :parameters (if data (apply #'slots `(t ,@data)) nil)
                                                             :cookie-jar *cookie-jar* :want-stream t)
                                      (values (st-json:getjso "id" *pm-reply*)
                                              *pm-reply*))))
                                
                                ((eql instr :retrieve)
                                 `(defmethod ,access-name
                                      ((inst (eql :retrieve)) &key (id nil)) ;; &allow-other-keys)
                                    (unless id
                                      (error "ID is required to retrieve a resource by identifier."))
                                    (with-pm-request
                                        (drakma:http-request (pm-uri ,resource id)
                                                             :basic-authorization (list *paymill-key*) :method :get
                                                             :cookie-jar *cookie-jar* :want-stream t)
                                      *pm-reply*)))
                                ((eql instr :update)
                                 `(defmethod ,access-name
                                      ((inst (eql :update)) &key (id nil) (data nil))
                                    (unless (and id data)
                                      (error "ID and DATA must be supplied to UPDATE a resource."))
                                    (with-pm-request
                                        (drakma:http-request (pm-uri ,resource id)
                                                             :basic-authorization (list *paymill-key*) :method :put
                                                             :content (x-www-form-encode-pairs data) :cookie-jar *cookie-jar* :want-stream t)
                                      *pm-reply*)))
                                ((eql instr :delete)
                                 `(defmethod ,access-name
                                      ((inst (eql :delete)) &key (id nil))
                                    (unless id
                                      (error "ID must be supplied to DELETE a resource."))
                                    (with-pm-request
                                        (drakma:http-request (pm-uri ,resource id)
                                                             :basic-authorization (list *paymill-key*) :method :delete
                                                             :cookie-jar *cookie-jar* :want-stream t)
                                      *pm-reply*)))
                                ((eql instr :list)
                                 `(defmethod ,access-name
                                      ((inst (eql :list)) &key (slot nil) (ascending t))
                                    (let* ((qst (if slot (concatenate 'string "order="
                                                                      (string-downcase (symbol-name slot)) 
                                                                      (if ascending "_asc" "_desc")) ""))
                                           (uri (pm-quri ,resource qst)))
                                      
                                    (with-pm-request
                                        (drakma:http-request uri
                                                             :basic-authorization (list *paymill-key*) :method :get
                                                             :cookie-jar *cookie-jar* :want-stream t)
                                      *pm-reply*))))
                                ((eql instr :refund)
                                 `(defmethod ,access-name
                                      ((inst (eql :list)) &key (id nil) (amount nil))
                                    (unless (and id amount)
                                      (error "A transaction ID and an AMOUNT are required to make a refund."))
                                    (let* ((qst (concatenate 'string "amount=" amount))
                                           (uri (pm-quri ,resource qst)))
                                      (with-pm-request
                                          (drakma:http-request uri
                                                               :basic-authorization (list *paymill-key*) :method :get
                                                               :cookie-jar *cookie-jar* :want-stream t)
                                        *pm-reply*))))
                                (t nil)))))))

