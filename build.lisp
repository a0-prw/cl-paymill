(in-package :paymill)

(defvar *pm-reply*)

(defun x-www-form-encode-pairs (pairs)
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

(define-condition api-internal-error (simple-error) 
  ((caller :initarg :caller :reader caller)
   (message :initarg :message :reader message))
  (:report (lambda (c s)
             (format s "~S signalled an error: ~A"
                     (caller c) (message c)))))

(defvar *cookie-jar* (make-instance 'drakma:cookie-jar))

(defun clear-cookie-jar ()
  (drakma:delete-old-cookies *cookie-jar*))

(defun resource-from-keyword (keyword)
  (string-downcase (symbol-name keyword)))

(defmacro with-pm-request (request-form &body body)
  "Does error checking of the reply from Paymill resulting from
  REQUEST-FORM.  If there are no errors, it binds (per-thread,
  locally) the variable *PM-REPLY* to the data associated with
  Paymill's reply for use in BODY, otherwise it signals 'an
  appropriate error'."
  (let ((reply (gensym "RP"))
        (status (gensym "ST"))
        (tmp (gensym "TMP")))
     `(multiple-value-bind (,reply ,status)
          ,request-form
        (if (= ,status 200)
            (let* ((,tmp (st-json:getjso "data" ,reply))
                   (*pm-reply* (if ,tmp ,tmp ,reply)))
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
resources.  See paymill.lisp for usage."
  (let ((access-name (intern (symbol-name resource) :paymill)))
    `(progn (defgeneric ,access-name (instruction &key &allow-other-keys))
            ,@(remove-if 
               #'null 
               (loop for instr in instr-list
                  collect (cond ((eql instr :new)
                                 `(defmethod ,access-name
                                      ((inst (eql :new)) &key  (data nil))
                                    (with-pm-request
                                        (drakma:http-request (pm-uri ,resource)
                                                             :basic-authorization `(,',*paymill-key*) :method :post
                                                             :parameters data :cookie-jar *cookie-jar* :want-stream t)
                                      (values (st-json:getjso "id" *pm-reply*)
                                              *pm-reply*))))
                                
                                ((eql instr :retrieve)
                                 `(defmethod ,access-name
                                      ((inst (eql :get)) &key (id nil)) ;; &allow-other-keys)
                                    (unless id
                                      (error "ID is required to retrieve a resource by identifier."))
                                    (with-pm-request
                                        (drakma:http-request (pm-uri ,resource id)
                                                             :basic-authorization `(,',*paymill-key*) :method :get
                                                             :cookie-jar *cookie-jar* :want-stream t)
                                      *pm-reply*)))
                                ((eql instr :update)
                                 `(defmethod ,access-name
                                      ((inst (eql :update)) &key (id nil) (data nil))
                                    (unless (and id data)
                                      (error "ID and DATA must be supplied to UPDATE a resource."))
                                    (with-pm-request
                                        (drakma:http-request (pm-uri ,resource id)
                                                             :basic-authorization `(,',*paymill-key*) :method :put
                                                             :content (x-www-form-encode-pairs data) :cookie-jar *cookie-jar* :want-stream t)
                                      *pm-reply*)))
                                ((eql instr :delete)
                                 `(defmethod ,access-name
                                      ((inst (eql :delete)) &key (id nil));; (data nil))
                                    (unless id
                                      (error "ID must be supplied to DELETE a resource."))
                                    (with-pm-request
                                        (drakma:http-request (pm-uri ,resource id)
                                                             :basic-authorization `(,',*paymill-key*) :method :delete
                                                             :cookie-jar *cookie-jar* :want-stream t)
                                      *pm-reply*)))
                                ((eql instr :list)
                                 `(defmethod ,access-name
                                      ((inst (eql :list)) &key (data nil) (slot nil) (ascending t))
                                    (let* ((qst (if slot (concatenate 'string "order="
                                                                      (string-downcase (symbol-name slot)) 
                                                                      (if ascending "_asc" "_desc")) ""))
                                           (uri (pm-quri ,resource qst)))
                                      
                                    (with-pm-request
                                        (drakma:http-request uri
                                                             :basic-authorization `(,',*paymill-key*) :method :get
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
                                                               :basic-authorization `(,',*paymill-key*) :method :get
                                                               :cookie-jar *cookie-jar* :want-stream t)
                                        *pm-reply*))))
                                (t nil)))))))
                                

