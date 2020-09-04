(module-name salesforce)

(import (com lmig joey http)
	(com lmig joey config)
	(srfi 69))

(define (get-sfdc-token)
  (let* ((login-url (config-ref 'sfdc-login-url))
	 (grant-service (config-ref 'sfdc-grant-service))
	 (full-login-url (string-append login-url grant-service))
	 (client-id (config-ref 'sfdc-client-id))
	 (client-secret (config-ref 'sfdc-client-secret))
	 (username (config-ref 'sfdc-username))
	 (password (config-ref 'sfdc-password))
	 (response (http-send-json *http-client* 'post
				   (string-append
				    full-login-url
				    "&client_id="
				    (url-encode client-id)
				    "&client_secret="
				    (url-encode client-secret)
				    "&username="
				    (url-encode username)
				    "&password="
				    (url-encode password)))))
    (if (= (response-status response) 200)
	(response-body/object response)
	#f)))

(define +sf-query-url-frag+ "/services/data/v41.0/query?q=")

(define (http-send-json/salesforce
	 client method token-data uri-append . optional-obj)
  (apply
   http-send-json/authenticate
   `(,client ,method
	     ,(string-append
	       (hash-table-ref token-data 'instance_url)
	       uri-append)
	     ,(hash-table-ref token-data 'access_token)
	     .
	     ,optional-obj)))

(define (soql-quote str)
  (let* ((escape-quotes
	  (lambda (s)
	    (string-map (lambda (c) (if (or (char=? c #\') (char=? c #\")) (string #\\ c) c))
			s))))
  (string-append "'" (escape-quotes str) "'")))

(define (soql-substitute-?s soql . params)
  (let loop ((s soql)
	     (p params))
    (cond
     ((null? p) s)
     (else
      (let ((loc (string-contains s "?")))
	    (if (not loc)
		s
		(let* ((s1 (substring s 0 loc))
		       (s2 (substring s (+ 1 loc) (string-length s)))
		       (next (car p)))
		  (loop
		   (string-append
		    s1
		    (cond
		     ((number? next) (number->string next))
		     ((string? next) (soql-quote next))
		     ((boolean? next) (if next "TRUE" "FALSE"))
		     (else (error "I don't know how to convert that to a SOQL constant")))
		    s2)
		   (cdr p)))))))))

(define (salesforce-run-query/raw soql . params)
  (let* ((final-soql (apply soql-substitute-?s `(,soql . ,params)))
	 (tok (get-sfdc-token)))
    (http-send-json/salesforce *http-client* 'get tok
			       (string-append +sf-query-url-frag+
					      (url-encode final-soql)))))


(define +soql-error-message+ "SOQL error: ")

(define (salesforce-run-query soql . params)
  (let* ((response (apply salesforce-run-query/raw `(,soql . ,params))))
    (if (= (response-status response) 200)
	(vector-ref
	 (hash-table-ref
	  (response-body/object response)
	  'records
	  (lambda () (error "no records found in SOQL query result")))
	 0)
	(error
	 (string-append
	  +soql-error-message+
	  (hash-table-ref
	   (vector-ref
	    (response-body/object response)
	    0)
	   'message
	   (lambda () "")))))))

;; a special version of hash-table-ref that lets us walk Salesforce child objects

(define (salesforce-hash-table-ref ht key)
  (let* ((key-components (string-split (symbol->string key) ".")))
    (let loop ((c key-components)
	       (v ht))
      (cond
       ((null? c) v)
       (else (loop (cdr c) (hash-table-ref v (string->symbol (car c)))))))))
